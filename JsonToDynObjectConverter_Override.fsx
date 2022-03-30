#r "nuget: DynamicObj, 1.0.1"
#r "nuget: Expecto, 9.0.4"

open DynamicObj
open Newtonsoft.Json
open System.IO
open Microsoft.FSharp.Core

type JsonParser = {
    TokenType: JsonToken
    Value: string option
} with
    static member create tokenType value = {
        TokenType = tokenType
        Value = value
    }

/// Fixed format function.
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
/// fixed print function *only necessary to pass "format" forward
let print (d:DynamicObj) = printfn "%s" (d |> format)


type DynamicObjConverter() =
    inherit JsonConverter<DynamicObj>()

    override this.ReadJson(reader : JsonReader, objectType : System.Type, existingValue : DynamicObj, hasExistingValue:bool, serializer : JsonSerializer) : DynamicObj =   
        /// The isInit parameter is necessary as the reader starts with the first value.
        /// But every iteration thereafter we need to progress the reader to the next value, with reader.next().
        let rec sortJsonParserArr (result: obj option) (isInit:bool) =
            let addValueToParentList(listObj:obj option) (value:'a) =
                /// unbox 'a does not seem to provide any benefit. When comparing output to manually created dyn object,
                /// it still needs to be boxed to be equal.
                let list = listObj.Value :?> obj seq |> Seq.map (fun x -> unbox<'a> x) |> List.ofSeq
                let res = (value::list) |> Seq.ofList
                sortJsonParserArr (Some res) false
            let next = if isInit then true else reader.Read()
            if next = false then 
                result
            else 
                let isList = result.IsSome && result.Value :? obj seq
                let currentJsonObj = JsonParser.create reader.TokenType (if isNull reader.Value then None else string reader.Value |> Some)
                // printfn "%A, %A" currentJsonObj.TokenType currentJsonObj.Value
                match currentJsonObj.TokenType with
                | JsonToken.StartObject ->
                    let obj = DynamicObj()
                    if isList then
                        let v = sortJsonParserArr (Some obj) false
                        addValueToParentList result v.Value
                    else
                        sortJsonParserArr (Some obj) false
                | JsonToken.EndObject -> 
                    result
                | JsonToken.StartArray ->
                    /// Need to use Sequence to be able to use any casting to and from: obj seq <-> 'a seq
                    let list: obj seq = Seq.empty
                    sortJsonParserArr (Some list) false
                | JsonToken.EndArray ->
                    let list = result.Value :?> obj seq |> List.ofSeq |> List.rev
                    Some list
                | JsonToken.PropertyName ->
                    let key = currentJsonObj.Value.Value
                    if result.IsNone then failwith "Cannot apply property without parent dyn object."
                    let parent = 
                        match result.Value with
                        | :? DynamicObj ->
                            let logger = result.Value :?> DynamicObj
                            let v = sortJsonParserArr None false
                            logger.SetValue(key, v.Value)
                            logger |> box
                        | _ -> failwith "Cannot parse parent type to supported types." 
                    sortJsonParserArr (Some parent) false
                | JsonToken.String -> 
                    let v = string currentJsonObj.Value.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Integer -> 
                    let v = int currentJsonObj.Value.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Float -> 
                    let v = float currentJsonObj.Value.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Boolean ->
                    let v = System.Boolean.Parse currentJsonObj.Value.Value
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
                    let v = string currentJsonObj.Value.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | any -> 
                    // printfn "CAREFUL! %A" currentJsonObj
                    sortJsonParserArr None false
        let res = sortJsonParserArr(None) true |> Option.get
        match res with
        | :? list<obj> as list ->
            let loggerList = list |> List.map (fun x -> unbox<DynamicObj> x)
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
            JsonConvert.SerializeObject(value, settings)
        writer.WriteRaw (v)

let toJson(dynObj:DynamicObj) = 
    JsonConvert.SerializeObject(dynObj, new DynamicObjConverter())

let ofJson(jsonSource:string) = JsonConvert.DeserializeObject<DynamicObj>(jsonSource, new DynamicObjConverter())

/// This function should always ONLY BE USED FOR TESTING!
/// THIS FUNCTION KILLS ANY WHITESPACE EVEN FROM JSON VALUES!
let minifyJson(json:string) = json.Replace(" ","").Split(System.Environment.NewLine)|> String.concat "" 

module UnitTests = 
    open Expecto

    let allTests =
        testList "JsonDynObjectConverter" [
            test "Test json string to dyn object compared to dyn object created by hand." {
                let simpleJson = """{"firstLevel": "test"}"""
                let dynObjOfJson = ofJson(simpleJson)
                dynObjOfJson |> print
                let dynObj = 
                    let l = DynamicObj()
                    l.SetValue("firstLevel", "test")
                    l
                dynObj |> print
                Expect.equal dynObjOfJson dynObj "Both dyn objects are expected to be equal."
            }
            test "Test json string to dyn object and back to json" {
                let simpleJson = minifyJson """{"firstLevel": "test"}"""
                let dynObjOfJson = ofJson(simpleJson)
                let revertToJson = toJson(dynObjOfJson)
                Expect.equal simpleJson revertToJson "Recreated Json, after being converted from to dyn object shoudl equal json source."
            }
            test "Test nested simple json object" {
                let json = minifyJson """{"firstLevel": {"name": "firstLevelName"}}"""
                let dynObjOfJson = ofJson json
                let revertToJson = toJson dynObjOfJson
                Expect.equal json revertToJson "Recreated Json equals json source with nested example."
            }
            test "Test json number types" {
                let json = minifyJson """{"depth": 2, "floatingBoat": 3.51}"""
                let dynObjOfJson = ofJson json 
                let revertToJson = toJson dynObjOfJson
                Expect.equal json revertToJson "Recreated Json equals json source with json number types."
            }
            test "Test 3-level nested json object with string and number json types" {
                let json = minifyJson """{"firstLevel": {"name": "firstLevelName","type": "object","firstLevelProperties": {"depth": 2,"floatingBoat": 3.51}}}"""
                let dynObjOfJson = ofJson(json)
                let revertToJson = toJson dynObjOfJson
                Expect.equal json revertToJson "Recreated Json equals json source with 3 level nested example."
            }
            test "Test Integer, float, bool, null json types" {
                let json = minifyJson """{"depth": 2,"floatingBoat": 3.51,"isTrue?": true,"isNull?": null}"""
                let dynObjOfJson = ofJson json 
                let revertToJson = toJson dynObjOfJson
                Expect.equal json revertToJson "Recreated Json equals json source with different json value types."
            }
            test "Test basic json array type." {
                let json = minifyJson """{"myfirstArray": ["value1", "value2", "value3"]}"""
                let dynObjOfJson = ofJson json
                let revertToJson = toJson dynObjOfJson
                Expect.equal json revertToJson "Recreated Json equals json source with different json value types."
            }
            test "Compare 'ofJson' to dyn obj created by hand, for json array type." {
                let simpleJson = """{"myfirstArray": ["value1", "value2", "value3"]}"""
                let dynObjOfJson = ofJson(simpleJson)
                let dynObj = 
                    let l = DynamicObj()
                    /// Sadly i am not able to avoid converting to 'obj list'.
                    let list: obj list = ["value1"; "value2"; "value3"]
                    l.SetValue("myfirstArray", list)
                    l
                Expect.equal dynObjOfJson dynObj "Both dyn objects are expected to be equal."
            }
            test "Test nested json array with object elements" {
                let json = minifyJson """{"myfirstArray": [{"name": "John","age": 30},{"name": "Mary","age": 25},{"name": "Peter","age": 20}]}"""
                let dynObjOfJson = ofJson json
                let revertToJson = toJson dynObjOfJson
                Expect.equal json revertToJson "Recreated Json equals json source with different json value types."
            }
            test "Test root level json array with object elements" {
                let json = minifyJson """[{"name": "John","age": 30},{"name": "Mary","age": 25},{"name": "Peter","age": 20}]"""
                let dynObjOfJson = ofJson json
                let revertToJson = 
                    let root = dynObjOfJson.TryGetValue("root") |> Option.get 
                    root :?> obj seq
                    |> Seq.map (fun x -> unbox<DynamicObj> x) 
                    |> Seq.map (fun x -> toJson x)
                    |> String.concat ","
                    |> sprintf "[%s]"
                    |> minifyJson
                Expect.equal json revertToJson "Recreated Json equals json source with different json value types."
            }
            test "Test empty json objects" {
                let json = minifyJson """{"name": {}}"""
                let dynObjOfJson = ofJson json
                let revertToJson = toJson dynObjOfJson
                Expect.equal json revertToJson "Recreated Json equals json source with empty json object."
            }
            test "Test nested prints" {
                let outer = DynamicObj()
                let inner = DynamicObj()
                inner.SetValue("Level", "Information")
                inner.SetValue("MessageTemplate","{Method} Request at {Path}")
                outer.SetValue("serilog", inner)
                let print =
                    try 
                        outer |> print
                        true 
                    with
                        | e -> false
                Expect.isTrue print "Expected to print nested object."
            }
        ]
    Expecto.Tests.runTests Impl.ExpectoConfig.defaultConfig allTests

let jsonSource = 
    let s = __SOURCE_DIRECTORY__
    let p = Path.Combine(s, @"files\dynObjectTest.json")
    File.ReadAllText(p)

// ofJson jsonSource |> print

// ofJson jsonSource |> toJson |> minifyJson = minifyJson jsonSource

let dynamicAccess (dynObject:DynamicObj)  (accessStr:string) =
    let toDynArr = accessStr.Split(".")
    let rec access (ind:int) (dynArr:string []) result =
        if ind >= dynArr.Length then
            result
        elif ind <> 0 && result = None then
            None
        else
            let parentObj = if ind = 0 then dynObject else box result.Value :?> DynamicObj
            let next = parentObj.TryGetValue(dynArr.[ind])
            access (ind+1) dynArr next
    access 0 toDynArr None

open System.Text.RegularExpressions

/// 1. negative lookbehind: (?<!(/|\\)) -> No / or \ before {
/// 2. must start with: {
/// 3. capture named group 'value' : (?<value>.+?(?!(/|\\))); careful \<value> does not show as comment, better look at code.
/// 4. group contains any number of wildcard characters except { AND }, minimum 1 but as few as possible: [^\{}]+?
/// 5. negative lookahead: (?!(/|\\)) -> No / or \ before }
/// 6. must end with: }
let pattern = @"(?<!(/|\\)){(?<value>[^\{}]+?(?!(/|\\)))}"

let getDynamicAccessStrings(input:string) = Regex.Matches(input, pattern) |> Array.ofSeq    

let readDynObjInFormatString(dynObj:DynamicObj,formatString:string) =
    /// Need replacerList to store arbitrary guids and actual dynamic access values. 
    /// The Guids are used as temporary replacements to remove escaped curly braces, without accidentally touching any inserted dynamic values.
    let mutable replacerList: (string*string) list = []
    let evaluator = 
        MatchEvaluator (fun m -> 
            let dynAccessResult = dynamicAccess (dynObj) m.Groups.["value"].Value
            let dynAccessResultString =
                if dynAccessResult.IsSome then 
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
    Regex.Replace(formatString, pattern, evaluator)
    // Update escaped curly braces to normal curly braces
    |> removeEscapedCurlyBraces
    // replace guids with actual dynamic access values
    |> replaceTempGuids
    
module UnitTestsDynamicAccess =
    open Expecto

    let allTests =
        testList "JsonDynObjectConverter" [
            test "Test simple dynamic access" {
                let simpleJson = """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
                let dynObjOfJson = ofJson(simpleJson)
                let dynamicAccessPort = dynamicAccess dynObjOfJson  "myLog.Request.Port"
                Expect.equal dynamicAccessPort (Some "8085") "Expected to get port value."
            }
            test "Test access string pattern with simple access string" {
                let formatString = """{myLog.Request.Path}"""
                let accessString = getDynamicAccessStrings(formatString) |> Array.head
                Expect.equal accessString.Groups.["value"].Value ("myLog.Request.Path") "Should match and return access string."
            }
            test "Test access string pattern in more complex formatting string" {
                let formatString = """Logging {myLog.Request.Path} @ some time point."""
                let accessString = getDynamicAccessStrings(formatString) |> Array.head
                Expect.equal accessString.Groups.["value"].Value ("myLog.Request.Path") "Should match and return access string."
            }
            test "Test access string pattern with multiple access string" {
                let formatString = """Logging {myLog.Request.Path} @ {myLog.Timestamp}."""
                let accessString = getDynamicAccessStrings(formatString)
                Expect.equal accessString.[0].Groups.["value"].Value ("myLog.Request.Path") "Should match and return first access string."
                Expect.equal accessString.[1].Groups.["value"].Value ("myLog.Timestamp") "Should match and return second access string."
            }
            test "Test access string pattern with escaped curly only." {
                let formatString = """Testing escaped /{curles/}."""
                let accessString = getDynamicAccessStrings(formatString)
                Expect.equal accessString (Array.empty) "Should match and return access string."
            }
            test "Test access string pattern with empty non-escaped curly only." {
                let formatString = """Hello i am just fooling around {}"""
                let accessString = getDynamicAccessStrings(formatString)
                Expect.equal accessString (Array.empty) "Should match and return access string."
            }
            test "Test access string pattern with complext access string." {
                let formatString = """Logging {myLog.Request.Path} @ {myLog.Timestamp}. {} Request solved for {myLog.Response.StatusCode} /{{myLog.Response.Time}/}. Testing escaped /{curles/}."""
                let accessStrings = getDynamicAccessStrings(formatString)
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
                let dynObjOfJson = ofJson json
                let result = """Testing escaped {curly} {boys}. And another \{boy\}. Now a mixed up {curly boy}."""
                let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
                Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
            }
            test "Test if values with escaped curly braces are still escaped." {
                let json = """{"Key": "This is my value with /{escaped/} curly braces."}"""
                let formatString = """The following value should still contain escaped curly braces: {Key}"""
                let dynObjOfJson = ofJson json
                let result = """The following value should still contain escaped curly braces: This is my value with /{escaped/} curly braces."""
                let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
                Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
            }
            test "Test read DynObj into complex formatString" {
                let json = """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Response": {"StatusCode": "200","Time": "00:00:14.3531003"}, "Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
                let formatString = """Logging "{myLog.Request.Path}" @ {myLog.Timestamp}. {} Request solved for {myLog.Response.StatusCode} /{time: {myLog.Response.Time}/}. Testing escaped \{curly\} /{boys/}."""
                let dynObjOfJson = ofJson json
                let result = """Logging "/api/IHelpdeskAPI/checkCaptcha" @ 2022.03.28 07:45:10.00949. {} Request solved for 200 {time: 00:00:14.3531003}. Testing escaped {curly} {boys}."""
                let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
                Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
            }
        ]
    Expecto.Tests.runTests Impl.ExpectoConfig.defaultConfig allTests

let formatString = """Logging "{myLog.Request.Path}" @ {myLog.Timestamp}. {} Request solved for {myLog.Response.StatusCode} /{time: {myLog.Response.Time}/}. Testing escaped \{curly\} /{boys/}."""

readDynObjInFormatString(ofJson jsonSource, formatString)