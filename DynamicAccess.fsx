#r "nuget: DynamicObj, 1.0.1"
#r "nuget: Expecto, 9.0.4"

open Newtonsoft.Json
open DynamicObj

type DynamicObjConverter() =
    inherit JsonConverter<DynamicObj>()

    override this.ReadJson(reader : JsonReader, objectType : System.Type, existingValue : DynamicObj, hasExistingValue:bool, serializer : JsonSerializer) : DynamicObj = 
       
        /// The isInit parameter is necessary as the reader starts with the first value.
        /// But every iteration thereafter we need to progress the reader to the next value, with reader.next().
        let rec readJsonParserFieldToDynObj (result: obj option) (isInit:bool) =
            let addValueToParentList(listObj:obj option) (value:'a) =
                /// unbox 'a does not seem to provide any benefit. When comparing output to manually created dyn object,
                /// it still needs to be boxed to be equal.
                let list = listObj.Value :?> obj seq |> Seq.map (fun x -> unbox<'a> x) |> List.ofSeq
                let res = (value::list) |> Seq.ofList
                readJsonParserFieldToDynObj (Some res) false
            let next = isInit || reader.Read()
            if next = false then 
                result
            else 
                let isList = result.IsSome && result.Value :? obj seq
                let tokenType = reader.TokenType
                let tokenValue = (if isNull reader.Value then None else string reader.Value |> Some)
                //printfn "%A, %A" tokenType tokenValue
                match tokenType with
                | JsonToken.StartObject ->
                    let obj = DynamicObj()
                    if isList then
                        let v = readJsonParserFieldToDynObj (Some obj) false
                        addValueToParentList result v.Value
                    else
                        readJsonParserFieldToDynObj (Some obj) false
                | JsonToken.EndObject -> 
                    result
                | JsonToken.StartArray ->
                    /// Need to use Sequence to be able to use any casting to and from: obj seq <-> 'a seq
                    let list: obj seq = Seq.empty
                    readJsonParserFieldToDynObj (Some list) false
                | JsonToken.EndArray ->
                    let list = result.Value :?> obj seq |> List.ofSeq |> List.rev
                    Some list
                | JsonToken.PropertyName ->
                    let key = tokenValue.Value
                    if result.IsNone then failwith "Cannot apply property without parent dyn object."
                    let parent = 
                        match result.Value with
                        | :? DynamicObj ->
                            let logger = result.Value :?> DynamicObj
                            let v = readJsonParserFieldToDynObj None false
                            logger.SetValue(key, v.Value)
                            logger |> box
                        | _ -> failwith "Cannot parse parent type to supported types." 
                    readJsonParserFieldToDynObj (Some parent) false
                | JsonToken.String -> 
                    let v = string tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Integer -> 
                    let v = int tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Float -> 
                    let v = float tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Boolean ->
                    let v = System.Boolean.Parse tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Null ->
                    let v = None
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                // TODO!
                | JsonToken.Bytes | JsonToken.Date ->
                    let v = string tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | any -> 
                    // printfn "CAREFUL! %A" currentJsonObj
                    readJsonParserFieldToDynObj None false
        let res = readJsonParserFieldToDynObj(None) true |> Option.get
        match res with
        | :? list<obj> as list ->
            let loggerList = list
            let r = DynamicObj()
            r.SetValue("root", loggerList)
            r
        | :? DynamicObj as root ->
            root
        | _ -> failwith "Could not parse Result to any supported type."

    override this.WriteJson(writer : JsonWriter, value : DynamicObj, serializer : JsonSerializer) =
        let v =
            let settings = 
                let s = JsonSerializerSettings()
                s.ReferenceLoopHandling <- ReferenceLoopHandling.Serialize
                s
            let hasRootArr = value.TryGetValue "root"
            if hasRootArr.IsSome then
                hasRootArr.Value 
                |> fun v -> JsonConvert.SerializeObject(v, settings)
            else
                JsonConvert.SerializeObject(value, settings)
        writer.WriteRaw (v)

module DynamicObj =

    let toJson(dynObj:DynamicObj) = JsonConvert.SerializeObject(dynObj, new DynamicObjConverter())

    let ofJson(jsonSource:string) = JsonConvert.DeserializeObject<DynamicObj>(jsonSource, new DynamicObjConverter())

    /// Fixed format function from DynamicObj Repository
    let format (d:DynamicObj) =
    
        let members = d.GetDynamicMemberNames() |> Seq.cast<string> |> List.ofSeq
        let rec loop (object:DynamicObj) (identationLevel:int) (membersLeft:string list) (acc:string list) =
            let ident = [for i in 0 .. identationLevel-1 do yield "    "] |> String.concat ""
            match membersLeft with
            | [] -> acc |> List.rev |> String.concat System.Environment.NewLine
            | m::rest ->
                let item = object?(``m``)
                match item with
                | :? DynamicObj as item -> 
                    let innerMembers = item.GetDynamicMemberNames() |> Seq.cast<string> |> List.ofSeq
                    let innerPrint = (loop item (identationLevel + 1) innerMembers [])
                    loop object identationLevel rest ($"{ident}?{m}:{System.Environment.NewLine}{innerPrint}" :: acc)
                | _ -> 
                    loop object identationLevel rest ($"{ident}?{m}: {item}"::acc)

        loop d 0 members []


open System
open System.Text.RegularExpressions
open DynamicObj

module Regex =
    
    /// 1. negative lookbehind: (?<!(/|\\)) -> No / or \ before {
    /// 2. must start with: {
    /// 3. capture named group 'value' : (?<value>.+?(?!(/|\\))); careful \<value> does not show as comment, better look at code.
    /// 4. group contains any number of wildcard characters except { AND }, minimum 1 but as few as possible: [^\{}]+?
    /// 5. negative lookahead: (?!(/|\\)) -> No / or \ before }
    /// 6. must end with: }
    [<Literal>]
    let internal Pattern = @"(?<!(/|\\)){(?<value>[^\{}]+?(?!(/|\\)))}"

    let getDynamicAccessStrings (input: string) = [| for i in Regex.Matches(input, Pattern) -> i |] 

/// Use "." syntax to access nested objects. Use "^?" to access sequence header.
/// **Example**: "Request.Body^?"
let dynamicAccess (accessStr:string) (dynObject:DynamicObj) =
    let toDynArr = accessStr.Split([|"."|], StringSplitOptions.RemoveEmptyEntries)
    let rec access (ind:int) (dynArr:string []) result =
        if ind >= dynArr.Length then
            result
        elif ind <> 0 && result = None then
            None
        else
            // when starting the start object is the parent
            let parentObj = if ind = 0 then dynObject else box result.Value :?> DynamicObj
            // next will be the result if it is the last step of the dynArr. 
            // If it is not, it MUST be a dynamic object (Thats why parentObj will be casted to DynamicObj in the next rec iteration).
            // If the current dynArr-step ends with "^?" the user assumes it is a sequence at this step and wants access to the seq head. In this case we cast to seq<'a>.
            let currentDynArr, isSeqHead = 
                let cda0 = dynArr.[ind]
                let isSeqHead = cda0.EndsWith "^?" 
                let cda = if isSeqHead then cda0.[..cda0.Length-3] else cda0
                cda, isSeqHead
            let next = 
                let next0 = parentObj.TryGetValue(currentDynArr)
                next0 |> Option.map (fun (n: obj) -> if isSeqHead then n :?> seq<obj> |> Seq.head else n) 
            access (ind+1) dynArr next
    access 0 toDynArr None

let readDynObjInFormatString(dynObj:DynamicObj,formatString:string) =
    /// Need replacerList to store artificial guids and actual dynamic access values. 
    /// The Guids are used as temporary replacements to remove escaped curly braces, without accidentally touching any inserted dynamic values.
    let mutable replacerList: (string*string) list = []
    let evaluator = 
        MatchEvaluator (fun m -> 
            let dynAccessResult = dynamicAccess m.Groups.["value"].Value (dynObj) 
            let dynAccessResultString =
                if dynAccessResult.IsSome then
                    match dynAccessResult.Value with
                    | :? DynamicObj as d -> printfn "1"; DynamicObj.toJson d
                    | :? seq<obj> as dseq -> printfn "2"; dseq |> Seq.map (fun x -> x :?> DynamicObj |> DynamicObj.toJson) |> String.concat "; "
                    | _ -> 
                        let t = dynAccessResult.Value.GetType()
                        printfn "%A" t.FullName
                        dynAccessResult.Value.ToString()
                else
                    "None"
            let newGuid = System.Guid.NewGuid().ToString()
            // save both guid and actual value in replacerList. 
            replacerList <- (newGuid,dynAccessResultString)::replacerList
            // return guid to replace dynamic access string
            newGuid
        )
    let removeEscapedCurlyBraces(str:string) = 
        Regex.Replace(str, @"(\\{|/{)", @"{")
        |> fun x -> Regex.Replace(x, @"(\\}|/})", @"}")
    let replaceTempGuids(str:string) = 
        let mutable res = str
        replacerList |> List.iter (fun (guid,value) -> 
            res <- Regex.Replace(res, guid, value)
        )
        res
    // replace dyn access string with random guids, stored with actual values in replacerList
    Regex.Replace(formatString, Regex.Pattern, evaluator)
    // Update escaped curly braces to normal curly braces
    |> removeEscapedCurlyBraces
    // replace guids with actual dynamic access values
    |> replaceTempGuids

/// This function should always ONLY BE USED FOR TESTING!
/// THIS FUNCTION KILLS ANY WHITESPACE EVEN FROM JSON VALUES!
let private minifyJson(json:string) = 
    json
        .Replace(" ","")
        // if i add this line, my tests break
        // .Replace("\n",System.Environment.NewLine)
        .Replace(System.Environment.NewLine,"")


let testLogJson = 
    minifyJson """{"Timestamp":"2022.05.03 07:28:11.82714","Request":{"Path":"/api/IHelpdeskAPI/getCaptcha","PathBase":"","Method":"GET","Host":"localhost","Port":8085,"QueryString":"","Query":null,"Headers":{"Connection":"close","Content-Type":"application/json; charset=utf-8","Accept":"*/*","Accept-Encoding":"gzip, deflate, br","Accept-Language":"en-GB,en-US;q=0.9,en;q=0.8","Cookie":"ajs_anonymous_id=%22bf0866e7-3877-4d25-8805-c1b2a4b5bd71%22; isDarkmode=false","Host":"localhost:8085","Referer":"http://localhost:8080/","User-Agent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36 OPR/85.0.4341.75","sec-fetch-dest":"empty","sec-fetch-mode":"cors","sec-fetch-site":"same-origin","sec-ch-ua-platform":"\"Windows\"","sec-ch-ua-mobile":"?0","x-remoting-proxy":"true","sec-ch-ua":"\" Not A;Brand\";v=\"99\", \"Chromium\";v=\"99\", \"Opera\";v=\"85\""},"UserAgent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36 OPR/85.0.4341.75","ContentType":"application/json; charset=utf-8","Body":[{"Name":"Sam","Age":28,"Size":1.84}]},"Response":{"StatusCode":200,"Time":"00:00:00.0000627"}}"""

open Expecto

let dynObjOfJson = DynamicObj.ofJson(testLogJson)

readDynObjInFormatString (dynObjOfJson,"Test: {Request.Body}")

let dynamic_access_tests =
    testList "dynamic access tests" [
        test "Test simple dynamic access" {
            let simpleJson = """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
            let dynObjOfJson = DynamicObj.ofJson(simpleJson)
            let dynamicAccessPort = dynamicAccess "myLog.Request.Port" dynObjOfJson
            Expect.equal dynamicAccessPort (Some "8085") "Expected to get port value."
        }
        test "Test access string pattern with simple access string" {
            let formatString = """{myLog.Request.Path}"""
            let accessString = Regex.getDynamicAccessStrings(formatString) |> Array.head
            Expect.equal accessString.Groups.["value"].Value ("myLog.Request.Path") "Should match and return access string."
        }
        test "Test access string pattern in more complex formatting string" {
            let formatString = """Logging {myLog.Request.Path} @ some time point."""
            let accessString = Regex.getDynamicAccessStrings(formatString) |> Array.head
            Expect.equal accessString.Groups.["value"].Value ("myLog.Request.Path") "Should match and return access string."
        }
        test "Test access string pattern with multiple access string" {
            let formatString = """Logging {myLog.Request.Path} @ {myLog.Timestamp}."""
            let accessString = Regex.getDynamicAccessStrings(formatString)
            Expect.equal accessString.[0].Groups.["value"].Value ("myLog.Request.Path") "Should match and return first access string."
            Expect.equal accessString.[1].Groups.["value"].Value ("myLog.Timestamp") "Should match and return second access string."
        }
        test "Test access string pattern with escaped curly only." {
            let formatString = """Testing escaped /{curles/}."""
            let accessString = Regex.getDynamicAccessStrings(formatString)
            Expect.equal accessString (Array.empty) "Should match and return access string."
        }
        test "Test access string pattern with empty non-escaped curly only." {
            let formatString = """Hello i am just fooling around {}"""
            let accessString = Regex.getDynamicAccessStrings(formatString)
            Expect.equal accessString (Array.empty) "Should match and return access string."
        }
        test "Test access string pattern with complext access string." {
            let formatString = """Logging {myLog.Request.Path} @ {myLog.Timestamp}. {} Request solved for {myLog.Response.StatusCode} /{{myLog.Response.Time}/}. Testing escaped /{curles/}."""
            let accessStrings = Regex.getDynamicAccessStrings(formatString)
            printfn "%A" accessStrings
            Expect.equal accessStrings.Length 4 "Should return 4 access strings."
            Expect.equal accessStrings.[0].Groups.["value"].Value ("myLog.Request.Path") "Should match and return 'myLog.Request.Path' access string."
            Expect.equal accessStrings.[1].Groups.["value"].Value ("myLog.Timestamp") "Should match and return 'myLog.Timestamp' access string."
            Expect.equal accessStrings.[2].Groups.["value"].Value ("myLog.Response.StatusCode") "Should match and return 'myLog.Response.StatusCode' access string."
            Expect.equal accessStrings.[3].Groups.["value"].Value ("myLog.Response.Time") "Should match and return 'myLog.Response.Time' access string."
        }
        test "Test correct escape of curly braces." {
            let json = """{"Key": "Value"}"""
            let formatString = """Testing escaped \{curly\} /{boys/}. And another \\{boy\\}. Now a mixed up \{curly boy/}."""
            let dynObjOfJson = DynamicObj.ofJson json
            let result = """Testing escaped {curly} {boys}. And another \{boy\}. Now a mixed up {curly boy}."""
            let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
            Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
        }
        test "Test if values with escaped curly braces are still escaped." {
            let json = """{"Key": "This is my value with /{escaped/} curly braces."}"""
            let formatString = """The following value should still contain escaped curly braces: {Key}"""
            let dynObjOfJson = DynamicObj.ofJson json
            let result = """The following value should still contain escaped curly braces: This is my value with /{escaped/} curly braces."""
            let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
            Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
        }
        test "Test read DynObj into complex formatString" {
            let json = """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Response": {"StatusCode": "200","Time": "00:00:14.3531003"}, "Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
            let formatString = """Logging "{myLog.Request.Path}" @ {myLog.Timestamp}. {} Request solved for {myLog.Response.StatusCode} /{time: {myLog.Response.Time}/}. Testing escaped \{curly\} /{boys/}."""
            let dynObjOfJson = DynamicObj.ofJson json
            let result = """Logging "/api/IHelpdeskAPI/checkCaptcha" @ 2022.03.28 07:45:10.00949. {} Request solved for 200 {time: 00:00:14.3531003}. Testing escaped {curly} {boys}."""
            let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
            Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
        }
        //test "Test dynamicAccess with seq header logic" {
        //    let logger = Logger.OfJson(testLogJson)
        //    logger.DynamicAccess ""
        //}
    ]
Expecto.Tests.runTests Impl.ExpectoConfig.defaultConfig dynamic_access_tests