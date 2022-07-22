// This implementation is taken from Paket

open System
open System.Globalization
open System.Text.RegularExpressions

/// Contains active patterns which allow to deal with [Semantic Versioning](http://semver.org/) (SemVer).
module SemVerActivePattern =
    let (|ParseRegex|_|) pattern input  =
        let m = Regex.Match(input, pattern, RegexOptions.ExplicitCapture)

        match m.Success with
        | true ->
            Some (List.tail [ for g in m.Groups -> g.Value ])
        | false ->
            None

    let (|SemVer|_|) version =
        let pattern =
            @"^(?<major>\d+)" +
            @"(\.(?<minor>\d+))?" +
            @"(\.(?<patch>\d+))?" +
            @"(\-(?<pre>[0-9A-Za-z\-\.]+))?" +
            @"(\+(?<build>[0-9A-Za-z\-\.]+))?$"

        match version with
        | ParseRegex pattern [major; minor; patch; pre; build] ->
            Some [major; minor; patch; pre; build]
        | _ ->
            None

    let (|ValidVersion|_|) = function
        | null | "" -> None
        | ver when ver.Length > 1 && ver.StartsWith("0") -> None
        | _ -> Some ValidVersion


[<CustomEquality; CustomComparison>]
type PreReleaseSegment = 
    | AlphaNumeric of string
    | Numeric of bigint

    member x.CompareTo(y) =
        match x, y with
        | AlphaNumeric a, AlphaNumeric b -> compare a b
        | Numeric a, Numeric b -> compare a b
        | AlphaNumeric _, Numeric _ -> 1
        | Numeric _, AlphaNumeric _ -> -1

    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? PreReleaseSegment as y -> x.CompareTo(y)                
            | _ -> invalidArg "yobj" "can't compare to other types of objects."
            
    override x.GetHashCode() = hash x
    
    member x.Equals(y) =
        match x, y with
        | AlphaNumeric a, AlphaNumeric b -> a = b
        | Numeric a, Numeric b -> a = b
        | AlphaNumeric _, Numeric _ -> false
        | Numeric _, AlphaNumeric _ -> false

    override x.Equals yobj = 
        match yobj with 
        | :? PreReleaseSegment as y -> x.Equals(y)
        | _ -> false

/// Information about PreRelease packages.
[<CustomEquality; CustomComparison>]
type PreRelease = 
    { Origin : string
      Name : string
      Values : PreReleaseSegment list }
      
    static member TryParse (str : string) = 
        if String.IsNullOrEmpty str then None
        else
            let getName fromList =
                match fromList with
                | AlphaNumeric(a)::_ -> a
                | _::AlphaNumeric(a)::_ -> a // fallback to 2nd
                | _ -> ""
                
            let parse (segment: string) =
                match bigint.TryParse segment with
                | true, number when number >= 0I -> Numeric number
                | _ -> AlphaNumeric segment
                
            let notEmpty = StringSplitOptions.RemoveEmptyEntries
            let name, values = 
                match str.Split([|'.'|],notEmpty) with
                | [|one|] -> 
                    let list = one.Split([|'-'|],notEmpty) |> Array.map parse |> List.ofArray
                    
                    // semver1-like embedded / inlined prerelease numbers
                    let name =
                        match Regex("^(?<name>[a-zA-Z]+)").Match(one) with 
                        | ex when ex.Success -> ex.Value
                        | _ -> getName list
                        
                    name, list
                                    
                | multiple -> //semver2: dashes are ok, inline numbers not
                
                    let list = multiple |> Array.map parse |> List.ofArray
                    getName list, list
                    
            Some { Origin = str; Name = name; Values = values }

    member x.Equals(y) = x.Origin = y.Origin

    override x.Equals(yobj) = 
        match yobj with
        | :? PreRelease as y -> x.Equals(y)
        | _ -> false
        
    override x.ToString() = x.Origin
    
    override x.GetHashCode() = hash x.Origin
    
    member x.CompareTo(yobj) = 
        let rec cmp item count xlist ylist = 
            if item < count then
                let res = compare (List.head xlist) (List.head ylist)
                if res = 0 then 
                    cmp (item + 1) count (List.tail xlist) (List.tail ylist)
                else
                    res // result given by first difference
            else
                sign xlist.Length - ylist.Length // https://semver.org/#spec-item-11
        let len = min x.Values.Length yobj.Values.Length // compare up to common len
        cmp 0 len x.Values yobj.Values
        
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? PreRelease as y -> x.CompareTo(y)
            | _ -> invalidArg "yobj" "PreRelease: cannot compare to values of different types"


/// Contains the version information. For parsing use [SemVer.parse](fake-core-semver.html)
/// 
/// > Note: If you use `{ version with Patch = myPath; Original = None }` to overwrite some parts of this string make sure to overwrite `Original` to `None` in order to recalculate the version string.
/// 
/// > Note: For overwriting the `PreRelease` part use: `{ Version with Original = None; PreRelease = PreRelease.TryParse "alpha.1" }`
[<CustomEquality; CustomComparison; StructuredFormatDisplay("{AsString}")>]
type SemVerInfo = 
    { /// MAJOR version when you make incompatible API changes.
      Major : uint32
      /// MINOR version when you add functionality in a backwards-compatible manner.
      Minor : uint32
      /// PATCH version when you make backwards-compatible bug fixes.
      Patch : uint32
      /// The optional PreRelease version
      PreRelease : PreRelease option
      /// The optional build no.
      Build : bigint
      BuildMetaData : string
      // The original version text
      Original : string option }
    
    member x.Normalize() = 
        let build = 
            if x.Build > 0I then ("." + x.Build.ToString("D")) else ""
                        
        let pre = 
            match x.PreRelease with
            | Some preRelease -> ("-" + preRelease.Origin)
            | None -> ""

        sprintf "%d.%d.%d%s%s" x.Major x.Minor x.Patch build pre

    member x.NormalizeToShorter() = 
        let s = x.Normalize()
        let s2 = sprintf "%d.%d" x.Major x.Minor
        if s = s2 + ".0" then s2 else s

    override x.ToString() = 
        match x.Original with
        | Some version -> version.Trim()
        | None -> x.Normalize()
    
    member x.AsString
        with get() = x.ToString()
        
    member x.Equals(y) =
        x.Major = y.Major && x.Minor = y.Minor && x.Patch = y.Patch && x.Build = y.Build && x.PreRelease = y.PreRelease

    override x.Equals(yobj) = 
        match yobj with
        | :? SemVerInfo as y -> x.Equals(y)
        | _ -> false
    
    override x.GetHashCode() = hash (x.Major, x.Minor, x.Patch, x.Build, x.PreRelease)
    
    member x.CompareTo(y) =
        let comparison =  
            match compare x.Major y.Major with 
            | 0 ->
                match compare x.Minor y.Minor with
                | 0 ->
                    match compare x.Patch y.Patch with
                    | 0 ->  
                        match compare x.Build y.Build with 
                        | 0 -> 
                            match x.PreRelease, y.PreRelease with
                            | None, None -> 0
                            | Some _, None -> -1
                            | None, Some p -> 1
                            | Some p, Some p2 when p.Origin = "prerelease" && p2.Origin = "prerelease" -> 0
                            | Some p, _ when p.Origin = "prerelease" -> -1
                            | _, Some p when p.Origin = "prerelease" -> 1
                            | Some left, Some right -> compare left right
                        | c -> c
                    | c -> c
                | c -> c
            | c -> c
        comparison
    
    interface System.IComparable with
        member x.CompareTo yobj = 
            match yobj with
            | :? SemVerInfo as y -> x.CompareTo(y)
            | _ -> invalidArg "yobj" "SemVerInfo: cannot compare to values of different types"

///  Parser which allows to deal with [Semantic Versioning](http://semver.org/) (SemVer).
///  Make sure to read the documentation in the [SemVerInfo](fake-core-semverinfo.html) record as well if you manually create versions.
[<RequireQualifiedAccess>]
module SemVer =
    open System.Numerics
    open SemVerActivePattern
  
    /// Returns true if input appears to be a parsable semver string
    let isValid version =
        match version with
        | SemVer [ValidVersion major; ValidVersion minor; ValidVersion patch; pre; build] ->
            true
        | _ ->
            false

    /// Matches if str is convertible to Int and not less than zero, and returns the value as UInt.
    let inline private (|Int|_|) (str: string) =
        match Int32.TryParse (str, NumberStyles.Integer, null) with
        | true, num when num > -1 -> Some num
        | _ -> None
        
    /// Matches if str is convertible to big int and not less than zero, and returns the bigint value.
    let inline private (|Big|_|) (str: string) =
        match BigInteger.TryParse (str, NumberStyles.Integer, null) with
        | true, big when big > -1I -> Some big
        | _ -> None

    /// Splits the given version string by possible delimiters but keeps them as parts of resulting list.
    let private expand delimiter (text : string) =
        let sb = Text.StringBuilder()
        let res = seq {
            for ch in text do
                match List.contains ch delimiter with
                | true -> 
                    yield sb.ToString()
                    sb.Clear() |> ignore
                    yield ch.ToString()
                | false ->
                    sb.Append(ch) |> ignore
            if sb.Length > 0 then
                yield sb.ToString()
                sb.Clear() |> ignore
            }
        res |> Seq.toList
        
    let private validContent = Regex(@"(?in)^[a-z0-9-]+(\.[a-z0-9-]+)*")

    /// Parses the given version string into a SemVerInfo which can be printed using ToString() or compared
    /// according to the rules described in the [SemVer docs](http://semver.org/).
    /// ## Sample
    ///
    ///     parse "1.0.0-rc.1"     < parse "1.0.0"          // true
    ///     parse "1.2.3-alpha"    > parse "1.2.2"          // true
    ///     parse "1.2.3-alpha2"   > parse "1.2.3-alpha"    // true
    ///     parse "1.2.3-alpha002" > parse "1.2.3-alpha1"   // false
    ///     parse "1.5.0-beta.2"   > parse "1.5.0-rc.1"     // false
    let parse (version : string) = 
        try
            /// sanity check to make sure that all of the integers in the string are positive.
            /// because we use raw substrings with dashes this is very complex :(
            for s in version.Split([|'.'|]) do
                match Int32.TryParse s with 
                | true, s when s < 0 -> failwith "no negatives!" 
                | _ -> ignore ()  // non-numeric parts are valid

            if version.Contains("!") then 
                failwithf "Invalid character found in %s" version
            if version.Contains("..") then 
                failwithf "Empty version part found in %s" version

            let plusIndex = version.IndexOf("+")

            let versionStr = 
                match plusIndex with
                | n when n < 0 -> version
                | n -> version.Substring(0, n)

            /// there can only be one piece of build metadata, and it is signified by + sign
            /// and then any number of dot-separated alpha-numeric groups.
            let buildmeta =
                match plusIndex with
                | -1 -> ""
                | n when n = version.Length - 1 -> ""
                | n -> 
                    let content = validContent.Match(version.Substring(n + 1))
                    if content.Success then content.Value else ""

            let fragments = expand [ '.'; '-' ] versionStr
            /// matches over list of the version fragments *and* delimiters
            let major, minor, patch, revision, suffix =
                match fragments with
                | (Int M)::"."::(Int m)::"."::(Int p)::"."::(Big b)::tail -> M, m, p, b, tail
                | (Int M)::"."::(Int m)::"."::(Int p)::tail -> M, m, p, 0I, tail
                | (Int M)::"."::(Int m)::tail -> M, m, 0, 0I, tail
                | (Int M)::tail -> M, 0, 0, 0I, tail
                | _ -> raise(ArgumentException("SemVer.Parse", "version"))
                //this is expected to fail, for now :/
                //| [text] -> 0, 0, 0, 0I, [text] 
                //| [] | _ -> 0, 0, 0, 0I, []
            
            /// recreate the remaining string to parse as prerelease segments
            let prerelease =
                if suffix.IsEmpty || suffix.Tail.IsEmpty then ""
                else String.Concat(suffix.Tail).TrimEnd([|'.'; '-'|])

            { Major = uint32 major
              Minor = uint32 minor
              Patch = uint32 patch
              Build = revision
              PreRelease = PreRelease.TryParse prerelease
              BuildMetaData = buildmeta
              Original = Some version }

        with
        | e ->
            raise <| exn(sprintf "Can't parse \"%s\"." version, e)

open System
open System.Collections.Generic

/// Returns if the string is null or empty
let inline isNullOrEmpty value = String.IsNullOrEmpty value

let inline isNullOrWhiteSpace value = isNullOrEmpty value || value |> Seq.forall Char.IsWhiteSpace

/// Trims the given string
let inline trimChars (chars: char []) (x : string) = 
    if isNullOrEmpty x then x
    else x.Trim chars

/// Splits the given string at the given string delimiter
let inline splitStr (delimiterStr : string) (text : string) = 
    text.Split([| delimiterStr |], StringSplitOptions.None) |> Array.toList

/// Trims the given string
let inline trim (x : string) = 
    if isNullOrEmpty x then x
    else x.Trim()

/// Returns if the string is not null or empty
let inline isNotNullOrEmpty value = String.IsNullOrEmpty value |> not

/// Checks whether the given text starts with the given prefix
let startsWith (prefix: string) (text : string) = text.StartsWith prefix

/// Checks whether the given text starts with the given prefix
let inline (<*) prefix text = startsWith prefix text

/// Trims the start of the given string
let inline trimStartChars (chars: char []) (x : string) =
    if isNullOrEmpty x then x
    else x.TrimStart chars

/// Trims the end of the given string
let inline trimEndChars (chars: char[]) (x : string) =
    if isNullOrEmpty x then x
    else x.TrimEnd chars

/// Contains the parsed information of the release notes text file.
type ReleaseNotes =
    { /// The parsed version.
      AssemblyVersion: string
      /// The nuget package version.
      NugetVersion: string
      /// Semantic version
      SemVer: SemVerInfo
      /// Release date
      Date : DateTime option
      // The parsed release notes.
      Notes: string list }
    override x.ToString() = sprintf "%A" x

    static member New(assemblyVersion,nugetVersion,date,notes) = { 
        AssemblyVersion = assemblyVersion
        NugetVersion = nugetVersion
        SemVer = SemVer.parse nugetVersion
        Date = date
        Notes = notes }

    static member New(assemblyVersion,nugetVersion,notes) = ReleaseNotes.New(assemblyVersion,nugetVersion,None,notes)

/// [omit]
let private regexes = new Dictionary<_, _>()

/// [omit]
let getRegEx pattern = 
    match regexes.TryGetValue pattern with
    | true, regex -> regex
    | _ -> (new System.Text.RegularExpressions.Regex(pattern))

let private parseVersions =
    // https://github.com/fsprojects/FAKE/issues/2557
    let nugetRegexLegacy = getRegEx @"([0-9]+.)+[0-9]+(-[a-zA-Z]+\d*)?(.[0-9]+)?"
    let nugetRegex = 
        /// From Fake.Core.SemVer
        let pattern =
            @"^(?<major>\d+)" +
            @"(\.(?<minor>\d+))?" +
            @"(\.(?<patch>\d+))?" +
            @"(\-(?<pre>[0-9A-Za-z\-\.]+))?" +
            @"(\+(?<build>[0-9A-Za-z\-\.]+))?$"
        getRegEx pattern
    let assemblyVersionRegex = getRegEx @"([0-9]+.)+[0-9]+"
    fun line ->
        let assemblyVersion = assemblyVersionRegex.Match line
        if not assemblyVersion.Success
        then failwithf "Unable to parse valid Assembly version from release notes (%s)." line

        let nugetVersion = 
            /// Must split by whitespace to try match start of line and end of line in SemVer regex pattern
            let nugetVersion = 
                line.Split(' ') 
                |> Array.tryPick (fun segment -> 
                  let m = segment.Trim() |> nugetRegex.Match
                  if m.Success then Some m else None
                )
            if nugetVersion.IsSome // if isSome then it must be Success, so no need to check for that
            then nugetVersion.Value
            else
                /// Adds support for "nugetRegexLegacy" after change to correct SemVer parsing.
                /// This should lead to the least disruption to users.
                let nugetVersionLegacy = nugetRegexLegacy.Match line
                if nugetVersionLegacy.Success
                then nugetVersionLegacy
                else failwithf "Unable to parse valid Nuget version from release notes (%s)." line
        assemblyVersion, nugetVersion

let parseDate =
    let dateRegex = getRegEx @"(19|20)\d\d([- /.])(0[1-9]|1[012]|[1-9])\2(0[1-9]|[12][0-9]|3[01]|[1-9])"
    fun line ->
        let possibleDate = dateRegex.Match line
        match possibleDate.Success with
        | false -> None
        | true ->
            match DateTime.TryParse possibleDate.Value with
            | false, _ -> None
            | true, x -> Some(x)


/// Parse simple release notes sequence
let private parseSimple (line: string) =
    let assemblyVersion, nugetVersion = parseVersions line
    let trimDot (s:string) = s.TrimEnd('.')
    /// Find nugetVersion index in line
    let nugetVersionIndex = line.IndexOf nugetVersion.Value
    let notes = 
        line.Substring (nugetVersionIndex + nugetVersion.Length)
        |> trimChars [|' '; '-'|]
        |> splitStr ". "
        |> List.map (trimDot >> trim)
        |> List.filter isNotNullOrEmpty
        |> List.map (fun x -> x + ".")
    ReleaseNotes.New(assemblyVersion.Value, nugetVersion.Value, None, notes)

/// Parse "complex" release notes text sequence
let private parseAllComplex (text: seq<string>) =
    let rec findNextNotesBlock text =
        let isHeader line = "##" <* line
        let rec findEnd notes text =
            match text with
            | [] -> notes,[]
            | h :: rest -> if isHeader h then notes,text else findEnd (h :: notes) rest

        match text with
        | [] -> None
        | h :: rest -> if isHeader h then Some(h,findEnd [] rest) else findNextNotesBlock rest

    let rec loop releaseNotes text =
        match findNextNotesBlock text with
        | Some(header,(notes, rest)) ->
            let assemblyVer, nugetVer = parseVersions header
            let date = parseDate header
            let newReleaseNotes = ReleaseNotes.New(assemblyVer.Value,nugetVer.Value,date,notes |> List.filter isNotNullOrEmpty |> List.rev)
            loop (newReleaseNotes::releaseNotes) rest
        | None -> releaseNotes

    let result = loop [] (text |> Seq.map (trimStartChars [|' '; '*'|] >> trimEndChars [|' '|]) |> Seq.toList)
    if List.isEmpty result then
        failwithf "release note files containing only top level headers are not allowed"
    else result

/// Parses a Release Notes text and returns all release notes.
///
/// ## Parameters
///  - `data` - Release notes text
let parseAll (data: seq<string>) = 
    let data = data |> Seq.toList |> List.filter (not << isNullOrWhiteSpace)
    match data with
    | [] -> failwith "Empty Release file."
    | h :: _ ->
        let (|Simple|Complex|Invalid|) = function '*' -> Simple | '#' -> Complex | _ -> Invalid
        let firstNonEmptyChar = h.Trim([|'-'; ' '|]).[0]
        match firstNonEmptyChar with
        | Simple -> 
            data 
            |> Seq.map parseSimple
            |> Seq.toList
        | Complex -> parseAllComplex data
        | Invalid -> failwith "Invalid Release Notes format."
        |> List.sortBy (fun x -> x.SemVer)
        |> List.rev

    
/// Parses a Release Notes text and returns the lastest release notes.
///
/// ## Parameters
///  - `data` - Release notes text
let parse (data: seq<string>) =
    match
        data
        |> parseAll
        |> Seq.tryHead with
    | Some head -> head
    | None -> failwithf "The release notes document was not valid, see https://fake.build/apidocs/v5/fake-core-releasenotes.html for the allowed formats"

/// Parses a Release Notes text file and returns the lastest release notes.
///
/// ## Parameters
///  - `fileName` - Release notes text file name
let load fileName =
    System.IO.File.ReadLines fileName
    |> parse


let releaseNotesLines = [
    "* 1.0.0 - Initial version"
    "* 1.1.0 - First change"
]
let releaseNotes = parse releaseNotesLines
