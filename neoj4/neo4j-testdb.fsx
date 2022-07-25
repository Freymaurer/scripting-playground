#r "nuget: Neo4j.Driver, 4.4.0"

open System
open System.IO
open Neo4j

open Neo4j.Driver
open System.Threading.Tasks

let driver = Neo4j.Driver.GraphDatabase.Driver("bolt://localhost:7687", Neo4j.Driver.AuthTokens.Basic("neo4j","test"))
let session = driver.AsyncSession(SessionConfigBuilder.ForDatabase "swatedb")

/// <summary>Standardized function to easily execute neo4j cypher query.</summary>
/// <param name="query">The cypher query string</param>
/// <param name="parameters">Map of key value pairs. Only use this if you used parameters, for example '$Name' in your query. In which case you need to provide `Map ["Name", value]`.</param>
/// <param name="resultAs">How to return query results. In the format of `(fun (record:IRecord) -> parsingFunction record)`.</param>
let runNEO4JQuery (query:string) (parameters:Map<string,'a> option) (resultAs:Func<IRecord,'T>) = 
    async {
        let! readOntologyies =
            if parameters.IsSome then
                // Cast a whole lot of types to expected types by neo4j driver
                let param =
                    parameters.Value 
                    |> Map.fold (fun s k v ->  
                        let kvp = Collections.Generic.KeyValuePair.Create(k, box v)
                        kvp::s
                    ) []
                    |> fun x -> Collections.Generic.Dictionary<string,obj>(x :> Collections.Generic.IEnumerable<_>)
                session.RunAsync(Query(query,param))
            else
                session.RunAsync(query)
            |> Async.AwaitTask
        let! ontologies = 
            readOntologyies.ToListAsync(resultAs)
            |> Async.AwaitTask
        return ontologies
    } |> Async.RunSynchronously


let query = 
        @"MATCH (o:Ontology)
        RETURN o" 

let spanTreeQuery = """MATCH (t:Term {accession: "MS:1000031"})
CALL apoc.path.spanningTree(t, {
    labelFilter: "+Term",
    minLevel: 1,
    maxLevel: 2,
    limit: 40
})
YIELD path
RETURN path;
"""

let runSimple (query:string) =
    session.RunAsync(query).Result
    |> fun x -> x.ToListAsync().Result

type Tree = {
    Nodes: INode list
    Relationships: IRelationship list
}

let getTree =
    runSimple spanTreeQuery 
    |> fun x -> x 
    |> Seq.map (fun (x :IRecord) -> 
        let path = x.["path"].As<IPath>()
        let relationship = [for i in 0 .. path.Relationships.Count-1 do yield path.Relationships.Item i ]
        let startNode = path.Start
        let endNode = path.End
        [startNode; endNode], relationship
    )
    |> List.ofSeq
    |> List.unzip
    |> fun (nodes, relationships) -> 
        {
            Nodes = nodes |> List.concat |> List.distinct
            Relationships = relationships |> List.concat |> List.distinct
        }

let showConnection =
    let r = getTree.Relationships.[0]
    let s = getTree.Nodes |> List.find (fun node -> node.Id = r.StartNodeId)
    let e = getTree.Nodes |> List.find (fun node -> node.Id = r.EndNodeId)
    printfn 
        "(%A {accession: %A}) -[:%s]-> (%A {accession: %A})" 
        s.Properties.["name"]
        s.Properties.["accession"]
        r.Type
        e.Properties.["name"]
        e.Properties.["accession"]
