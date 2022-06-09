#r "nuget: FSharp.Data"
#r "nuget: ISADotNet, 0.3.1-preview.1"

open System.IO
open FSharp.Data

/// Read .xlsx files to Byte []. Need to adapt path
let xlsxBytes = File.ReadAllBytes (Path.GetFullPath @"C:\Users\Freym\Desktop\Book1.xlsx")

// /// This was used to check if Byte [] can be parsed back to .xlsx without problems
// let ms = new MemoryStream(xlsxBytes)
// File.WriteAllBytes (Path.GetFullPath @"C:\Users\Freym\Desktop\Book2.xlsx", xlsxBytes)

/// This string needs to contain the basic path for the target api 
/// Hint: Check "src/Shared/Shared.fs" for Routbuilder function and for the api-type
[<LiteralAttribute>]
let CommonAPIStr = @"https://localhost:3000/api/IISADotNetCommonAPIv1/"

// /// I use this line (https://github.com/nfdi4plants/Swate/blob/developer/src/Server/Server.fs#L280) for the most simple version of a get request.
// /// The following function can be used to check server connection. Url needs to be adapted
// Http.RequestString("https://localhost:3000/test/test1")

// The following can be any apis you want to adress directly.
/// Post request with Byte [] -> string
let parseAPIPath = $"{CommonAPIStr}toAssayJSON"
/// Post request with int -> string
let testPath = $"{CommonAPIStr}testPostNumber"


Http.RequestString(
    testPath,
    body = HttpRequestBody.TextRequest """ [ 5 ] """,
    httpMethod = "POST"
)

let isaJSONAssay = 
    Http.RequestString(
        parseAPIPath,
        body = BinaryUpload xlsxBytes,
        httpMethod = "POST"
    )

let isaJSONAssayPrep = isaJSONAssay.Replace(@"\","")

printf $"{isaJSONAssayPrep}"

ISADotNet.Json.Assay.fromString isaJSONAssayPrep.[1..isaJSONAssayPrep.Length-2]