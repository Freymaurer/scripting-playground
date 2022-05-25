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
                // printfn "%A, %A" tokenType tokenValue
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
                    // printfn "CAREFUL! %A" any
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
                s.ReferenceLoopHandling <- ReferenceLoopHandling.Serialize // ReferenceLoopHandling.Serialize
                // s.PreserveReferencesHandling <- PreserveReferencesHandling.Objects // PreserveReferencesHandling.Objects
                s
            let hasRootArr = value.TryGetValue "root"
            if hasRootArr.IsSome then
                hasRootArr.Value 
                |> fun v -> JsonConvert.SerializeObject(v, settings)
            else
                JsonConvert.SerializeObject(value, settings)
        writer.WriteRaw (v)

open System
open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Converters

// // https://stackoverflow.com/questions/28150908/f-json-webapi-serialization-of-option-types
// // http://gorodinski.com/blog/2013/01/05/json-dot-net-type-converters-for-f-option-list-tuple/
// type OptionConverter() =
//     inherit JsonConverter()

//     override x.CanConvert(t) = 
//         t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

//     override x.WriteJson(writer, value, serializer) =
//         let value = 
//             if value = null then null
//             else 
//                 let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
//                 fields.[0]  
//         serializer.Serialize(writer, value)

//     override x.ReadJson(reader, t, existingValue, serializer) =        
//         let innerType = t.GetGenericArguments().[0]
//         let innerType = 
//             if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
//             else innerType
//         let value = serializer.Deserialize(reader, innerType)
//         let cases = FSharpType.GetUnionCases(t)
//         if value = null then FSharpValue.MakeUnion(cases.[0], [||])
//         else FSharpValue.MakeUnion(cases.[1], [|value|])

type IdiomaticDuConverter() = 
    inherit JsonConverter()
    
    [<Literal>]
    let discriminator = "__Case"
    let primitives = Set [ JsonToken.Boolean; JsonToken.Date; JsonToken.Float; JsonToken.Integer; JsonToken.Null; JsonToken.String ]

    let writeValue (value:obj) (serializer:JsonSerializer, writer : JsonWriter) =
        if value.GetType().IsPrimitive then writer.WriteValue value
        else serializer.Serialize(writer, value)

    let writeProperties (fields : obj array) (serializer:JsonSerializer, writer : JsonWriter) = 
        fields |> Array.iteri (fun index value -> 
                      writer.WritePropertyName(sprintf "Item%d" index)
                      (serializer, writer) |> writeValue value)
    
    let writeDiscriminator (name : string) (writer : JsonWriter) = 
        writer.WritePropertyName discriminator
        writer.WriteValue name
        
    override __.WriteJson(writer, value, serializer) = 
        let unionCases = FSharpType.GetUnionCases(value.GetType())
        let unionType = value.GetType()
        let case, fields = FSharpValue.GetUnionFields(value, unionType)
        let allCasesHaveValues = unionCases |> Seq.forall (fun c -> c.GetFields() |> Seq.length > 0)

        match unionCases.Length, fields, allCasesHaveValues with
        | 2, [||], false -> writer.WriteNull()
        | 1, [| singleValue |], _
        | 2, [| singleValue |], false -> (serializer, writer) |> writeValue singleValue
        | 1, fields, _
        | 2, fields, false -> 
            writer.WriteStartObject()
            (serializer, writer) |> writeProperties fields
            writer.WriteEndObject()
        | _ -> 
            writer.WriteStartObject()
            writer |> writeDiscriminator case.Name
            (serializer, writer) |> writeProperties fields
            writer.WriteEndObject()
    
    override __.ReadJson(reader, destinationType, _, _) = 
        let parts = 
            if reader.TokenType <> JsonToken.StartObject then [| (JsonToken.Undefined, obj()), (reader.TokenType, reader.Value) |]
            else 
                seq { 
                    yield! reader |> Seq.unfold (fun reader -> 
                                         if reader.Read() then Some((reader.TokenType, reader.Value), reader)
                                         else None)
                }
                |> Seq.takeWhile(fun (token, _) -> token <> JsonToken.EndObject)
                |> Seq.pairwise
                |> Seq.mapi (fun id value -> id, value)
                |> Seq.filter (fun (id, _) -> id % 2 = 0)
                |> Seq.map snd
                |> Seq.toArray
        
        let values = 
            parts
            |> Seq.filter (fun ((_, keyValue), _) -> keyValue <> (discriminator :> obj))
            |> Seq.map snd
            |> Seq.filter (fun (valueToken, _) -> primitives.Contains valueToken)
            |> Seq.map snd
            |> Seq.toArray
        
        let case = 
            let unionCases = FSharpType.GetUnionCases(destinationType)
            let unionCase =
                parts
                |> Seq.tryFind (fun ((_,keyValue), _) -> keyValue = (discriminator :> obj))
                |> Option.map (snd >> snd)
            match unionCase with
            | Some case -> unionCases |> Array.find (fun f -> f.Name :> obj = case)
            | None ->
                // implied union case
                match values with
                | [| null |] -> unionCases |> Array.find(fun c -> c.GetFields().Length = 0)
                | _ -> unionCases |> Array.find(fun c -> c.GetFields().Length > 0)
        
        let values = 
            case.GetFields()
            |> Seq.zip values
            |> Seq.map (fun (value, propertyInfo) -> Convert.ChangeType(value, propertyInfo.PropertyType))
            |> Seq.toArray
        
        FSharpValue.MakeUnion(case, values)
    
    override __.CanConvert(objectType) = FSharpType.IsUnion objectType

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

/// This function should always ONLY BE USED FOR TESTING!
/// THIS FUNCTION KILLS ANY WHITESPACE EVEN FROM JSON VALUES!
let minifyJson(json:string) = 
    json
        .Replace(" ","")
        // if i add this line, my tests break
        // .Replace("\n",System.Environment.NewLine)
        .Replace(System.Environment.NewLine,"")

// open System
// open System.Text.RegularExpressions
// open DynamicObj

// module Regex =
    
//     /// 1. negative lookbehind: (?<!(/|\\)) -> No / or \ before {
//     /// 2. must start with: {
//     /// 3. capture named group 'value' : (?<value>.+?(?!(/|\\))); careful \<value> does not show as comment, better look at code.
//     /// 4. group contains any number of wildcard characters except { AND }, minimum 1 but as few as possible: [^\{}]+?
//     /// 5. negative lookahead: (?!(/|\\)) -> No / or \ before }
//     /// 6. must end with: }
//     [<Literal>]
//     let internal Pattern = @"(?<!(/|\\)){(?<value>[^\{}]+?(?!(/|\\)))}"

//     let getDynamicAccessStrings (input: string) = [| for i in Regex.Matches(input, Pattern) -> i |] 

// /// Use "." syntax to access nested objects. Use "^?" to access sequence header.
// /// **Example**: "Request.Body^?"
// let dynamicAccess (accessStr:string) (dynObject:DynamicObj) =
//     let toDynArr = accessStr.Split([|"."|], StringSplitOptions.RemoveEmptyEntries)
//     let rec access (ind:int) (dynArr:string []) result =
//         if ind >= dynArr.Length then
//             result
//         elif ind <> 0 && result = None then
//             None
//         else
//             // when starting the start object is the parent
//             let parentObj = if ind = 0 then dynObject else box result.Value :?> DynamicObj
//             // next will be the result if it is the last step of the dynArr. 
//             // If it is not, it MUST be a dynamic object (Thats why parentObj will be casted to DynamicObj in the next rec iteration).
//             // If the current dynArr-step ends with "^?" the user assumes it is a sequence at this step and wants access to the seq head. In this case we cast to seq<'a>.
//             let currentDynArr, isSeqHead = 
//                 let cda0 = dynArr.[ind]
//                 let isSeqHead = cda0.EndsWith "^?" 
//                 let cda = if isSeqHead then cda0.[..cda0.Length-3] else cda0
//                 cda, isSeqHead
//             let next = 
//                 let next0 = parentObj.TryGetValue(currentDynArr)
//                 next0 |> Option.map (fun (n: obj) -> if isSeqHead then n :?> seq<obj> |> Seq.head else n) 
//             access (ind+1) dynArr next
//     access 0 toDynArr None

// let readDynObjInFormatString(dynObj:DynamicObj,formatString:string) =
//     /// Need replacerList to store artificial guids and actual dynamic access values. 
//     /// The Guids are used as temporary replacements to remove escaped curly braces, without accidentally touching any inserted dynamic values.
//     let mutable replacerList: (string*string) list = []
//     let evaluator = 
//         MatchEvaluator (fun m -> 
//             let dynAccessResult = dynamicAccess m.Groups.["value"].Value (dynObj) 
//             let dynAccessResultString =
//                 if dynAccessResult.IsSome then
//                     match dynAccessResult.Value with
//                     | :? DynamicObj as d -> printfn "1"; DynamicObj.toJson d
//                     | :? seq<obj> as dseq -> printfn "2"; dseq |> Seq.map (fun x -> x :?> DynamicObj |> DynamicObj.toJson) |> String.concat "; "
//                     | _ -> 
//                         let t = dynAccessResult.Value.GetType()
//                         printfn "%A" t.FullName
//                         dynAccessResult.Value.ToString()
//                 else
//                     "None"
//             let newGuid = System.Guid.NewGuid().ToString()
//             // save both guid and actual value in replacerList. 
//             replacerList <- (newGuid,dynAccessResultString)::replacerList
//             // return guid to replace dynamic access string
//             newGuid
//         )
//     let removeEscapedCurlyBraces(str:string) = 
//         Regex.Replace(str, @"(\\{|/{)", @"{")
//         |> fun x -> Regex.Replace(x, @"(\\}|/})", @"}")
//     let replaceTempGuids(str:string) = 
//         let mutable res = str
//         replacerList |> List.iter (fun (guid,value) -> 
//             res <- Regex.Replace(res, guid, value)
//         )
//         res
//     // replace dyn access string with random guids, stored with actual values in replacerList
//     Regex.Replace(formatString, Regex.Pattern, evaluator)
//     // Update escaped curly braces to normal curly braces
//     |> removeEscapedCurlyBraces
//     // replace guids with actual dynamic access values
//     |> replaceTempGuids


// let testLogJson = 
//     minifyJson """{"Timestamp":"2022.05.03 07:28:11.82714","Request":{"Path":"/api/IHelpdeskAPI/getCaptcha","PathBase":"","Method":"GET","Host":"localhost","Port":8085,"QueryString":"","Query":null,"Headers":{"Connection":"close","Content-Type":"application/json; charset=utf-8","Accept":"*/*","Accept-Encoding":"gzip, deflate, br","Accept-Language":"en-GB,en-US;q=0.9,en;q=0.8","Cookie":"ajs_anonymous_id=%22bf0866e7-3877-4d25-8805-c1b2a4b5bd71%22; isDarkmode=false","Host":"localhost:8085","Referer":"http://localhost:8080/","User-Agent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36 OPR/85.0.4341.75","sec-fetch-dest":"empty","sec-fetch-mode":"cors","sec-fetch-site":"same-origin","sec-ch-ua-platform":"\"Windows\"","sec-ch-ua-mobile":"?0","x-remoting-proxy":"true","sec-ch-ua":"\" Not A;Brand\";v=\"99\", \"Chromium\";v=\"99\", \"Opera\";v=\"85\""},"UserAgent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36 OPR/85.0.4341.75","ContentType":"application/json; charset=utf-8","Body":[{"Name":"Sam","Age":28,"Size":1.84}]},"Response":{"StatusCode":200,"Time":"00:00:00.0000627"}}"""

// open Expecto
// open System.Collections

// let dynObjOfJson = DynamicObj.ofJson(testLogJson)

// readDynObjInFormatString (dynObjOfJson,"Test: {Request.Body}")

// let dynamic_access_tests =
//     testList "dynamic access tests" [
//         test "Test simple dynamic access" {
//             let simpleJson = """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
//             let dynObjOfJson = DynamicObj.ofJson(simpleJson)
//             let dynamicAccessPort = dynamicAccess "myLog.Request.Port" dynObjOfJson
//             Expect.equal dynamicAccessPort (Some "8085") "Expected to get port value."
//         }
//         test "Test access string pattern with simple access string" {
//             let formatString = """{myLog.Request.Path}"""
//             let accessString = Regex.getDynamicAccessStrings(formatString) |> Array.head
//             Expect.equal accessString.Groups.["value"].Value ("myLog.Request.Path") "Should match and return access string."
//         }
//         test "Test access string pattern in more complex formatting string" {
//             let formatString = """Logging {myLog.Request.Path} @ some time point."""
//             let accessString = Regex.getDynamicAccessStrings(formatString) |> Array.head
//             Expect.equal accessString.Groups.["value"].Value ("myLog.Request.Path") "Should match and return access string."
//         }
//         test "Test access string pattern with multiple access string" {
//             let formatString = """Logging {myLog.Request.Path} @ {myLog.Timestamp}."""
//             let accessString = Regex.getDynamicAccessStrings(formatString)
//             Expect.equal accessString.[0].Groups.["value"].Value ("myLog.Request.Path") "Should match and return first access string."
//             Expect.equal accessString.[1].Groups.["value"].Value ("myLog.Timestamp") "Should match and return second access string."
//         }
//         test "Test access string pattern with escaped curly only." {
//             let formatString = """Testing escaped /{curles/}."""
//             let accessString = Regex.getDynamicAccessStrings(formatString)
//             Expect.equal accessString (Array.empty) "Should match and return access string."
//         }
//         test "Test access string pattern with empty non-escaped curly only." {
//             let formatString = """Hello i am just fooling around {}"""
//             let accessString = Regex.getDynamicAccessStrings(formatString)
//             Expect.equal accessString (Array.empty) "Should match and return access string."
//         }
//         test "Test access string pattern with complext access string." {
//             let formatString = """Logging {myLog.Request.Path} @ {myLog.Timestamp}. {} Request solved for {myLog.Response.StatusCode} /{{myLog.Response.Time}/}. Testing escaped /{curles/}."""
//             let accessStrings = Regex.getDynamicAccessStrings(formatString)
//             printfn "%A" accessStrings
//             Expect.equal accessStrings.Length 4 "Should return 4 access strings."
//             Expect.equal accessStrings.[0].Groups.["value"].Value ("myLog.Request.Path") "Should match and return 'myLog.Request.Path' access string."
//             Expect.equal accessStrings.[1].Groups.["value"].Value ("myLog.Timestamp") "Should match and return 'myLog.Timestamp' access string."
//             Expect.equal accessStrings.[2].Groups.["value"].Value ("myLog.Response.StatusCode") "Should match and return 'myLog.Response.StatusCode' access string."
//             Expect.equal accessStrings.[3].Groups.["value"].Value ("myLog.Response.Time") "Should match and return 'myLog.Response.Time' access string."
//         }
//         test "Test correct escape of curly braces." {
//             let json = """{"Key": "Value"}"""
//             let formatString = """Testing escaped \{curly\} /{boys/}. And another \\{boy\\}. Now a mixed up \{curly boy/}."""
//             let dynObjOfJson = DynamicObj.ofJson json
//             let result = """Testing escaped {curly} {boys}. And another \{boy\}. Now a mixed up {curly boy}."""
//             let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
//             Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
//         }
//         test "Test if values with escaped curly braces are still escaped." {
//             let json = """{"Key": "This is my value with /{escaped/} curly braces."}"""
//             let formatString = """The following value should still contain escaped curly braces: {Key}"""
//             let dynObjOfJson = DynamicObj.ofJson json
//             let result = """The following value should still contain escaped curly braces: This is my value with /{escaped/} curly braces."""
//             let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
//             Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
//         }
//         test "Test read DynObj into complex formatString" {
//             let json = """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Response": {"StatusCode": "200","Time": "00:00:14.3531003"}, "Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
//             let formatString = """Logging "{myLog.Request.Path}" @ {myLog.Timestamp}. {} Request solved for {myLog.Response.StatusCode} /{time: {myLog.Response.Time}/}. Testing escaped \{curly\} /{boys/}."""
//             let dynObjOfJson = DynamicObj.ofJson json
//             let result = """Logging "/api/IHelpdeskAPI/checkCaptcha" @ 2022.03.28 07:45:10.00949. {} Request solved for 200 {time: 00:00:14.3531003}. Testing escaped {curly} {boys}."""
//             let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
//             Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
//         }
//         //test "Test dynamicAccess with seq header logic" {
//         //    let logger = Logger.OfJson(testLogJson)
//         //    logger.DynamicAccess ""
//         //}
//     ]
// Expecto.Tests.runTests Impl.ExpectoConfig.defaultConfig dynamic_access_tests


// let isOption (p:System.Type) = 
//     p.IsGenericType &&
//     p.GetGenericTypeDefinition() = typedefof<Option<_>>

// let isSeq (p:System.Type) =
//     (
//         p.IsGenericType &&
//         (p.GetGenericTypeDefinition() = typedefof<seq<_>> || p.GetGenericTypeDefinition() = typedefof<List<_>>)
//     )
//     || p.IsArray

// // https://stackoverflow.com/questions/51976214/how-do-i-create-an-instance-of-an-f-option-type-using-activator-createinstance
// let makeOptionValue typey v isSome =
//     let optionType = typeof<unit option>.GetGenericTypeDefinition().MakeGenericType([| typey |])
//     let cases = FSharp.Reflection.FSharpType.GetUnionCases(optionType)
//     let cases = cases |> Array.partition (fun x -> x.Name = "Some")
//     let someCase = fst cases |> Array.exactlyOne
//     let noneCase = snd cases |> Array.exactlyOne
//     let relevantCase, args =
//         match isSome with
//         | true -> someCase, [| v |]
//         | false -> noneCase, [| |]
//     FSharp.Reflection.FSharpValue.MakeUnion(relevantCase, args)

// let (|IsList|_|) (candidate : obj) =
//     let t = candidate.GetType()
//     if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>
//     then Some (candidate :?> System.Collections.IEnumerable)
//     else None

// module rec Caster =

//     open System
//     open System.Reflection

//     let rec toOption (dObj0:obj) (t: System.Type) =
//         printfn "start toOption"
//         let innerType = t.GenericTypeArguments.[0]
//         // check if value is null
//         if isNull dObj0 then
//             makeOptionValue innerType null false
//         else
//             let v = matchType isOption innerType
//             makeOptionValue innerType v true

//     let rec toRecord (dObj0: obj) (t: System.Type) =
//         printfn "start toRecord"
//         match Reflection.FSharpType.IsRecord(t) with
//         | true -> 
//             let dObj = dObj0 :?> DynamicObj
//             let fieldValues: obj[] = 
//                 Reflection.FSharpType.GetRecordFields(t) 
//                 |> Array.map (fun field -> 
//                     let vo = dObj.TryGetValue field.Name
//                     matchType vo.Value field.PropertyType
//                 )
//             Reflection.FSharpValue.MakeRecord(t, fieldValues)
//         | false ->
//             failwithf "%A is not a record type." t.FullName

//     let rec toSeq (dObj0:obj) (t: System.Type) =
//         printfn "start toSeq"
//         let innerListType = if t.IsArray then t.GetElementType() else t.GenericTypeArguments.[0]
//         printfn "v: %A; innerListType: %A" dObj0 innerListType
//         dObj0
//         match dObj0 with
//         | IsList l0 ->
//             printfn "type1: %A" (l0.GetType().FullName)
//             let l = [for v0 in l0 -> v0]  // only used for length
//             let enumGetter = l0.GetEnumerator()
//             printfn "type2: %A" (l.GetType().FullName)

//             let genericList = typedefof<list<_>>
//             let constrcuted = genericList.MakeGenericType(innerListType)
//             let o = Activator.CreateInstance(constrcuted, [|l0|])
//             printfn "%A" (o.GetType().FullName)
//             let m = 
//                 Assembly.GetAssembly(typeof<list<_>>)
//                     .GetType("Microsoft.FSharp.Collections.ListModule")
//                     .GetMethod("Initialize")
//                     .MakeGenericMethod(innerListType)
//                     .Invoke(null, [| 
//                         Seq.length l; 
//                         fun (i: int) -> 
//                             printfn "%i" i
//                             match enumGetter.MoveNext() with
//                             | true -> 
//                                 let out = matchType enumGetter.Current innerListType
//                                 printfn "innerType: %A" (out.GetType().FullName)
//                                 "test"
//                             | false ->
//                                 failwith "sumthing went wrong"
//                     |]) 
//             printfn "type3: %A" (m.GetType().FullName)
//             m

//         | :? seq<_> as s -> 
//             printfn "2"
//             s
//             |> Seq.map (fun o -> matchType o innerListType)
//             |> box
//         | :? System.Array as a -> 
//             printfn "3"
//             a :> IEnumerable |> Seq.cast<_>
//             |> Seq.map (fun o -> matchType o innerListType)
//             |> Array.ofSeq
//             |> box
//         | _ -> 
//             failwithf "Could not match sequence type: %A" dObj0

//     let rec matchType (dObj0:obj) (t: System.Type) =
//         printfn "start matchType"
//         if Reflection.FSharpType.IsRecord t then
//             toRecord dObj0 t
//         elif isOption t then
//             toOption dObj0 t
//         elif isSeq t then
//             toSeq dObj0 t
//         else
//             // printfn "elseType: %A" (dObj0.GetType().FullName) 
//             dObj0 

/// Issues with:
/// - Nested options: option<option<'T>>
/// - sequence comparison
let inline AsRecordType<'a> (this:DynamicObj) =
    let js: string = DynamicObj.toJson this
    try 
        JsonConvert.DeserializeObject<'a>(js)
    with    
        | _ -> JsonConvert.DeserializeObject<'a>(js, IdiomaticDuConverter())


let simpleJson: string = """{"Name": "John", "Age": 42, "Size": 1.78, "Children": 2, "Debts": null, "IsLoggedIn": true, "Contact": {"Phone": "000000024", "Email": "JohnDoh@gmx.de"}, "Partner": null}"""
let nestedJson: string = """{"Name": "Sam", "Age": 54, "Size": 1.78, "Children": 1, "Debts": null, "IsLoggedIn": false, "Contact": {"Phone": "0000000023", "Email": "Sam_doh@gmx.de"}, "Partner": {"Name": "John", "Age": 42, "Size": 1.78, "Children": 2, "Debts": null, "IsLoggedIn": true, "Contact": {"Phone": "000000024", "Email": "JohnDoh@gmx.de"}, "Partner": null}}"""

type Contact = {
    Phone: string
    Email: string
}

type Person = {
    Name: string
    Age: int
    Size: float
    IsLoggedIn: bool
    Contact: Contact
    Children: int option
    Tries: int option
    Partner: Person option
}


module TestTypes = 
    type PersonPrimitiveTypes = {
        Name: string
        Age: int
        Size: float
        IsLoggedIn: bool
    }

    type PersonNestedRecords = {
        Name: string
        Age: int
        Size: float
        IsLoggedIn: bool
        Contact: Contact
    }

    type Person = {
        Name: string
        Age: int
        Size: float
        IsLoggedIn: bool
        Contact: Contact
        Children: int option
        Debts: float option
        Partner: Person option
    }

    type SequenceTestPrimitive = {
        StringList: string list
        StringSeq: string seq
        StringArray: string []
        IntList: int list
        IntSeq: int seq
        IntArray: int []
        FloatList: float list
        FloatSeq: float seq
        FloatArray: float []
        BoolList: bool list
        BoolSeq: bool seq
        BoolArray: bool [] 
    }

    let seqTestDynObjPrimitive =
        let d = DynamicObj()
        d.SetValue("StringList", ["a";"b";"c"])
        d.SetValue("StringSeq", seq ["a";"b";"c"])
        d.SetValue("StringArray", [|"a";"b";"c"|])
        d.SetValue("IntList", [1;2;3])
        d.SetValue("IntSeq", seq [1;2;3])
        d.SetValue("IntArray", [|1;2;3|])
        d.SetValue("FloatList", [1.1;2.1;3.1])
        d.SetValue("FloatSeq", seq [1.1;2.1;3.1])
        d.SetValue("FloatArray", [|1.1;2.1;3.1|])
        d.SetValue("BoolList", [true;false;true])
        d.SetValue("BoolSeq", seq [true;false;true])
        d.SetValue("BoolArray", [|true;false;true|])
        d

    type SequenceTest = {
        ContactList: Contact list
        ContactSeq: Contact seq
        ContactArray: Contact []
        OptionList:  (int option) list
        OptionSeq:   (int option) seq
        OptionArray: (int option) []
    }

    let c1 = {Phone = "12345"; Email = "anymail@hosting.com"}
    let c2 = {Phone = "23890745"; Email = "secondmail@hosting.com"}
    let c3 = {Phone = "23890745"; Email = "nomail@no.de"}
    let seqTestDynObj =
        let d = DynamicObj()
        d.SetValue("ContactList", [c1;c2;c3])
        d.SetValue("ContactSeq", seq [c1;c2;c3])
        d.SetValue("ContactArray", [|c1;c2;c3|])
        d.SetValue("OptionList", [Some(1);Some(2);None])
        d.SetValue("OptionSeq", seq [Some(1);Some(2);None])
        d.SetValue("OptionArray", [|Some(1);Some(2);None|])
        d

    type SequenceTestNested = {
        ContactList: Contact list list
        ContactSeq: Contact seq seq
        ContactArray: Contact [] []
        OptionList:  (int option) list list
        OptionSeq:   (int option) seq seq
        OptionArray: (int option) [] []
    }

    let seqTestDynObjNested =
        let d = DynamicObj()
        let c1 = {Phone = "12345"; Email = "anymail@hosting.com"}
        let c2 = {Phone = "23890745"; Email = "secondmail@hosting.com"}
        let c3 = {Phone = "23890745"; Email = "nomail@no.de"}
        d.SetValue("ContactList", [[c1;c2;c3]; [c2;c1;c3]])
        d.SetValue("ContactSeq", seq [seq [c1;c2;c3]; seq [c2;c1;c3]])
        d.SetValue("ContactArray", [|[|c1;c2;c3|]; [|c2;c1;c3|]|])
        d.SetValue("OptionList", [[Some(1);Some(2);None]; [Some(1);Some(2);None]])
        d.SetValue("OptionSeq", seq [seq [Some(1);Some(2);None]; seq [Some(1);Some(2);None]])
        d.SetValue("OptionArray", [|[|Some(1);Some(2);None|]; [|Some(1);Some(2);None|]|])
        d

    type SequenceTestCompositeNested = {
        OptionList:  (int option) list seq
        OptionSeq:   (int option) seq []
        OptionArray: (int option) [] list
    }

    let seqTestDynObjCompositeNested =
        let d = DynamicObj()
        d.SetValue("OptionList", seq [[Some(1);Some(2);None]; [Some(1);Some(2);None]])
        d.SetValue("OptionSeq",  [|seq [Some(1);Some(2);None]; seq [Some(1);Some(2);None]|])
        d.SetValue("OptionArray", [[|Some(1);Some(2);None|]; [|Some(1);Some(2);None|]])
        d

type OptionTestComplex = {
    RecordTypeOptionSome: Contact option
    RecordTypeOptionNone: Contact option
    ArrayOptionSome: seq<Contact> option
    ArrayOptionNone: Contact seq option
    /// will return null for inner option
    NestedOptionSome: Contact option option
    NestedOptionNone: Contact option option
}

let seqTestDynObjComplexOptions =
    /// MUST USE nested dynamic objects
    let c1 = 
        let d = DynamicObj()
        d.SetValue("Phone","1")
        d.SetValue("Email","email1")
        d
    // let c1 = {Phone = "12345"; Email = "email1"}    
    let d = DynamicObj()
    // even though this is will be casted to object, the current logic is meant to sit atop the "ofJson" logic,
    // which WILL create "Some 'a" as "'a" as it cannot know about the option type from json.
    // tl;dr: no Some for option types to test AsRecordType
    d.SetValue("RecordTypeOptionSome", c1)
    d.SetValue("RecordTypeOptionNone", None)
    d.SetValue("ArrayOptionSome", seq [c1])
    d.SetValue("ArrayOptionNone", seq [None])
    d.SetValue("NestedOptionSome", c1 |> Some |> Some)
    d

let primPerson = AsRecordType<OptionTestComplex> seqTestDynObjComplexOptions

/// F# Sequences are difficult to compare. Returns true if sequences are equal.
// See here: https://stackoverflow.com/questions/17101329/f-sequence-comparison
let compareSeq (seq1: seq<'a>) (seq2: seq<'a>) = 
    let c = Seq.compareWith (fun x y -> if x = y then 0 else 1) seq1 seq2
    if c = 0 then true else false

let primSeq = AsRecordType<TestTypes.SequenceTestNested> TestTypes.seqTestDynObjNested
compareSeq primSeq.OptionSeq [[Some(1);Some(2);None]; [Some(1);Some(2);None]]

open Expecto

let dynObject_to_fsharp_record_type =
    testList "dynObject to fsharp record type tests" [
        test "Test Simple of- and to json string convert" {
            let dynObjOfJson = DynamicObj.ofJson(simpleJson)
            let reverseConvert = DynamicObj.toJson dynObjOfJson
            Expect.equal (minifyJson reverseConvert) (minifyJson simpleJson) ""
        }
        test "Test Nested of- and to json string convert" {
            let dynObjOfJson = DynamicObj.ofJson(nestedJson)
            let reverseConvert = DynamicObj.toJson dynObjOfJson
            Expect.equal (minifyJson reverseConvert) (minifyJson nestedJson) ""
        }
        test "Primitive Types" {
            let dynObjOfJson = DynamicObj.ofJson(simpleJson)
            let primPerson = AsRecordType<TestTypes.PersonPrimitiveTypes> dynObjOfJson
            Expect.equal primPerson.Age 42 ""
            Expect.equal primPerson.Name "John" ""
            Expect.equal primPerson.Size 1.78 ""
            Expect.equal primPerson.IsLoggedIn true ""
        }
        test "Primitive Types anonymous" {
            let dynObjOfJson = DynamicObj.ofJson(simpleJson)
            let primAnoPerson = AsRecordType<{| Name: string; Age: int; Size: float; IsLoggedIn: bool|}> dynObjOfJson
            Expect.equal primAnoPerson.Age 42 ""
            Expect.equal primAnoPerson.Name "John" ""
            Expect.equal primAnoPerson.Size 1.78 ""
            Expect.equal primAnoPerson.IsLoggedIn true ""
        }
        test "Nested Record Types" {
            let dynObjOfJson = DynamicObj.ofJson(simpleJson)
            let primPerson = AsRecordType<TestTypes.PersonNestedRecords> dynObjOfJson
            Expect.equal primPerson.Age 42 ""
            Expect.equal primPerson.Name "John" ""
            Expect.equal primPerson.Size 1.78 ""
            Expect.equal primPerson.IsLoggedIn true ""
            Expect.equal primPerson.Contact.Email "JohnDoh@gmx.de" ""
            Expect.equal primPerson.Contact.Phone "000000024" ""
        }
        test "Option Types" {
            let dynObjOfJson = DynamicObj.ofJson(nestedJson)
            let primPerson = AsRecordType<TestTypes.Person> dynObjOfJson
            Expect.equal primPerson.Name "Sam" ""
            Expect.isTrue primPerson.Partner.IsSome ""
            Expect.equal primPerson.Partner.Value.Age 42 ""
            Expect.equal primPerson.Partner.Value.Name "John" ""
            Expect.equal primPerson.Partner.Value.Size 1.78 ""
            Expect.equal primPerson.Partner.Value.IsLoggedIn true ""
            Expect.equal primPerson.Partner.Value.Contact.Email "JohnDoh@gmx.de" ""
            Expect.equal primPerson.Partner.Value.Contact.Phone "000000024" ""
            Expect.equal primPerson.Debts None ""
            Expect.equal primPerson.Children (Some 1) ""
        }
        test "Primitive Sequence Types" {
            let primSeq = AsRecordType<TestTypes.SequenceTestPrimitive> TestTypes.seqTestDynObjPrimitive
            Expect.equal primSeq.StringList ["a";"b";"c"] "StringList"
            Expect.isTrue (compareSeq primSeq.StringSeq ["a";"b";"c"]) "StringSeq"
            Expect.equal primSeq.StringArray [|"a";"b";"c"|] "StringArray"
            Expect.equal primSeq.IntList [1;2;3] "IntList"
            Expect.isTrue (compareSeq primSeq.IntSeq [1;2;3]) "IntSeq"
            Expect.equal primSeq.IntArray [|1;2;3|] "IntArray"
            Expect.equal primSeq.FloatList [1.1;2.1;3.1] "FloatList"
            Expect.isTrue (compareSeq primSeq.FloatSeq [1.1;2.1;3.1]) "FloatSeq"
            Expect.equal primSeq.FloatArray [|1.1;2.1;3.1|] "FloatArray"
            Expect.equal primSeq.BoolList [true;false;true] "BoolList"
            Expect.isTrue (compareSeq primSeq.BoolSeq [true;false;true]) "BoolSeq"
            Expect.equal primSeq.BoolArray [|true;false;true|] "BoolArray"
        }
        // This test test also for explizit option types (in json case...some).
        test "Sequence Types" {
            let primSeq = AsRecordType<TestTypes.SequenceTest> TestTypes.seqTestDynObj
            Expect.equal primSeq.ContactList [TestTypes.c1;TestTypes.c2;TestTypes.c3] "ContactList"
            Expect.isTrue (compareSeq primSeq.ContactSeq [TestTypes.c1;TestTypes.c2;TestTypes.c3]) "ContactSeq"
            Expect.equal primSeq.ContactArray [|TestTypes.c1;TestTypes.c2;TestTypes.c3|] "ContactArray"
            Expect.equal primSeq.OptionList [Some(1);Some(2);None] "OptionList"
            Expect.isTrue (compareSeq primSeq.OptionSeq [Some(1);Some(2);None]) "OptionSeq"
            Expect.equal primSeq.OptionArray [|Some(1);Some(2);None|] "OptionArray"
        }
        test "Sequence Types Nested" {
            let primSeq = AsRecordType<TestTypes.SequenceTestNested> TestTypes.seqTestDynObjNested
            Expect.equal primSeq.OptionList [[Some(1);Some(2);None];[Some(1);Some(2);None]] "OptionList"
            // would need to match inner seq for equality
            // Expect.isTrue (compareSeq primSeq.OptionSeq [Seq.ofList [Some(1);Some(2);None]; Seq.ofList [Some(1);Some(2);None]]) "OptionSeq"
            Expect.equal primSeq.OptionArray [|[|Some(1);Some(2);None|]; [|Some(1);Some(2);None|]|] "OptionArray"
            Expect.equal primSeq.ContactList [[TestTypes.c1;TestTypes.c2;TestTypes.c3];[TestTypes.c2;TestTypes.c1;TestTypes.c3]] "ContactList"
        }
        test "Sequence Types Composite Nested" {
            let primSeq = AsRecordType<TestTypes.SequenceTestCompositeNested> TestTypes.seqTestDynObjCompositeNested
            Expect.equal primSeq.OptionArray [[|Some(1);Some(2);None|];[|Some(1);Some(2);None|]]  ""
        }

    ]
Expecto.Tests.runTestsWithCLIArgs [] [||] dynObject_to_fsharp_record_type
