#r "nuget: Expecto, 9.0.4"
#r "nuget: Fake.Core.Target, 5.23.0-alpha002"
#r "nuget: Fake.Core.ReleaseNotes, 5.23.0-alpha002"

open Fake.Core

type SemVerInfo with
    member this.ToSemVer() =
        let build = if this.Build > 0I then ("." + this.Build.ToString("D")) else ""

        let pre = 
            match this.PreRelease with
            | Some preRelease -> ("-" + preRelease.Origin)
            | None -> ""   
        
        let meta = 
            match this.BuildMetaData with
            | "" -> ""
            | metaData -> "+" + metaData

        sprintf "%d.%d.%d%s%s%s" this.Major this.Minor this.Patch build pre meta

module SemVerInfo =
    let toSemVer (semVer:SemVerInfo) = semVer.ToSemVer()

type SemVerRelease =
| Major
| Minor
| Patch
| Pre of string
| WIP

let updateSemVer (semVerReleaseType:SemVerRelease) (newestCommitHash:string) (prevSemVer:SemVerInfo) =
    match semVerReleaseType with
    | Major ->
        { prevSemVer with
            Major = prevSemVer.Major + 1u
            Minor = 0u
            Patch = 0u
            PreRelease = None
            Build = 0I
            BuildMetaData = newestCommitHash.Trim('#') }
    | Minor ->
        { prevSemVer with
            Minor = prevSemVer.Minor + 1u
            Patch = 0u
            PreRelease = None
            Build = 0I
            BuildMetaData = newestCommitHash.Trim('#') }
    | Patch ->
        { prevSemVer with
            Patch = prevSemVer.Patch + 1u
            PreRelease = None
            Build = 0I
            BuildMetaData = newestCommitHash.Trim('#') }
    | Pre preRelease -> 
        { prevSemVer with
            PreRelease = PreRelease.TryParse preRelease
            Build = 0I
            BuildMetaData = newestCommitHash.Trim('#') }
    | WIP ->
        { prevSemVer with
            BuildMetaData = newestCommitHash.Trim('#') }

let matchSemVerArg (args0: string list) =
    let args = args0 |> List.map (fun x -> x.ToLower().Trim()) 
    let opt = args |> List.tryFind (fun x -> x.StartsWith "semver:")
    match opt with
    | Some "semver:major"->
        Trace.trace "Increase major for next release notes."
        Major
    | Some "semver:minor" ->
        Trace.trace "Increase minor for next release notes."
        Minor
    | Some "semver:patch" ->
        Trace.trace "Increase patch for next release notes."
        Patch
    | Some isPre when isPre.StartsWith "semver:pre-" -> 
        Pre <| isPre.Replace("semver:pre-", "")
    | Some x ->
        Trace.traceError (sprintf "Unrecognized argument: \"%s\". Default to \"semver:wip\"." x)
        WIP
    | None | Some "semver:wip" ->
        Trace.trace "Add new commits to current release."
        WIP

let createDateString (dt:System.DateTime) = sprintf "%i-%i-%i" dt.Year dt.Month dt.Day

type ReleaseNotes.ReleaseNotes with
    /// Writes ReleaseNotes to established format. includes DateTime, SemVer, and Notes.
    member this.ComposeNotes() =
        [
            sprintf "### %s (Released %s)" (SemVerInfo.toSemVer this.SemVer) (this.Date |> Option.defaultValue System.DateTime.Now |> createDateString)
            yield! this.Notes
        ]

    static member initReleaseNotes() =
        ReleaseNotes.ReleaseNotes.New("0.0.0","0.0.0", Some System.DateTime.Now, [
                "* Additions:"
                "    * Initial set up for RELEASE_Notes.md"
            ])

/// Checks if RELEASE_NOTES.md exists and if not creates it.
let ensure() =
    let isExisting = Fake.IO.File.exists "RELEASE_NOTES.md"
    if isExisting = false then
        let newReleaseNotes = ReleaseNotes.ReleaseNotes.initReleaseNotes().ComposeNotes()
        Fake.IO.File.create "RELEASE_NOTES.md"
        Fake.IO.File.write
            true
            "RELEASE_NOTES.md"
            newReleaseNotes
        Trace.traceImportant "RELEASE_Notes.md created"
    else
        Trace.trace "RELEASE_Notes.md found"


open Expecto
[<Tests>]
let tests_SemVer =
    let semVer_1 = SemVer.parse "5.23.7-alpha.01+b0216ab"
    testList "SemVer" [
        testCase "Update SemVer Major" <| fun _ ->
            let result = updateSemVer Major "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "6.0.0+NewHash00a29f" ""

        testCase "Update SemVer Minor" <| fun _ ->
            let result = updateSemVer Minor "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "5.24.0+NewHash00a29f" ""

        testCase "Update SemVer Patch" <| fun _ ->
            let result = updateSemVer Patch "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "5.23.8+NewHash00a29f" ""

        testCase "Update SemVer Pre" <| fun _ ->
            let result = updateSemVer (Pre "alpha.02") "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "5.23.7-alpha.02+NewHash00a29f" ""
        testCase "Update SemVer WIP" <| fun _ ->
            let result = updateSemVer WIP "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "5.23.7-alpha.01+NewHash00a29f" ""
    ]

[<Tests>]
let tests_SemVerArgs =
    testList "SemVerArgs" [
        testCase "Test SemVerArg :major" <| fun _ ->
            let result = ["semver:major"] |> matchSemVerArg 
            Expect.equal result Major ""
        testCase "Test SemVerArg :minor" <| fun _ ->
            let result = ["semver:minor"] |> matchSemVerArg 
            Expect.equal result Minor ""
        testCase "Test SemVerArg :patch" <| fun _ ->
            let result = ["semver:patch"] |> matchSemVerArg 
            Expect.equal result Patch ""
        testCase "Test SemVerArg :pre-alpha.02" <| fun _ ->
            let result = ["semver:pre-alpha.02"] |> matchSemVerArg 
            Expect.equal result (Pre "alpha.02") ""
    ]

[<Tests>]
let tests_ReleaseNotes_Extensions =
    testList "SemVerArgs" [
        testCase "ReleaseNotes.ReleaseNotes.initReleaseNotes" <| fun _ ->
            let newNotes = ReleaseNotes.ReleaseNotes.initReleaseNotes()
            Expect.equal (newNotes.SemVer.ToSemVer()) "0.0.0" "newNotes.SemVer.ToSemVer()"
    ]

let all = testList "all" [
    tests_SemVer
    tests_SemVerArgs
    tests_ReleaseNotes_Extensions
]

Expecto.Tests.runTestsWithCLIArgs [] [||] all