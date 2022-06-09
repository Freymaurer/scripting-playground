#r "nuget: FSharp.Data"

open FSharp.Data

/// This string needs to contain the basic path for the target api 
/// Hint: Check "src/Shared/Shared.fs" for Routbuilder function and for the api-type
[<LiteralAttribute>]
let CommonAPIStr = @"https://swate.nfdi4plants.de/api/IISADotNetCommonAPIv1/"


// /// I use this line (https://github.com/nfdi4plants/Swate/blob/developer/src/Server/Server.fs#L280) for the most simple version of a get request.
// /// The following function can be used to check server connection. Url needs to be adapted
// Http.RequestString("https://localhost:3000/test/test1")

/// Post request with int -> string
let testPath = $"{CommonAPIStr}testPostNumber"


module OntologyAPI =

    [<LiteralAttribute>]
    let OntologyAPIStr = @"https://swate.nfdi4plants.de/api/IOntologyAPIv1/"

    let getTestNumber = $"{OntologyAPIStr}getTestNumber"

    let getAllTermsByParentTerm = $"{OntologyAPIStr}getAllTermsByParentTerm"


Http.RequestString(
    testPath,
    body = HttpRequestBody.TextRequest """ [ 5 ] """,
    httpMethod = "POST"
)

Http.RequestString(
    OntologyAPI.getTestNumber,
    httpMethod = "GET"
)

// https://github.com/nfdi4plants/Swate/blob/05bb3d73980b6fb68ca2b441078c7f38b72bc88e/src/Shared/TermTypes.fs#L24
/// This type will change with the upcoming Swate update
type Term = {
    Ontology        : string
    Accession       : string
    Name            : string
    Definition      : string
    XRefValueType   : string option
    IsObsolete      : bool
}

open FSharp.Data.JsonExtensions

/// <summary>Creates HTTP request to production SWATE API. Requesting all child terms to a parent term.</summary>
/// <param name="name">The term name of the parent term. For example **"instrument model"**</param>
/// <param name="str">The unique identifier/accession of the parent term. For example **"MS:1000031"**</param>
/// <returns>An array of all child terms.</returns>
let getTermsByParentTerm (name: string) (accession: string) =
    let res = Http.RequestString(
        OntologyAPI.getAllTermsByParentTerm,
        body = HttpRequestBody.TextRequest (sprintf """ [ { "Name": "%s", "TermAccession": "%s" } ] """ name accession),
        httpMethod = "POST"
    )
    [| for term in JsonValue.Parse(res) do
            let sourceOntology = term?OntologyName
            let accession = term?Accession
            let name = term?Name
            let definition = term?Definition
            let xRefValueType = term?XRefValueType
            let isObsolete = term?IsObsolete
            {
                Ontology      =  sourceOntology.AsString();
                Accession     =  accession.AsString();
                Name          =  name.AsString();
                Definition    =  definition.AsString();
                XRefValueType =  if xRefValueType.AsString() <> "" then xRefValueType.AsString() |> Some else None;
                IsObsolete    =  isObsolete.AsBoolean()
            }
    |]

getTermsByParentTerm "instrument model" "MS:1000031"
