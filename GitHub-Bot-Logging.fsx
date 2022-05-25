#r "nuget: LibGit2Sharp, 0.26.2"

open LibGit2Sharp
open System.IO

let url = @"https://github.com/James-the-Bot/Helddesk-Logs.git"
let path = @"C:\Users\Kevin\source\repos\LoggingTest"
let userName = "James-the-Bot"

let credentials = 
    let ch = 
        let userPw = new UsernamePasswordCredentials()
        userPw.Password <- "Access-Token-Placeholder"
        userPw.Username <- userName
        userPw :> Credentials
    new Handlers.CredentialsHandler(fun url user cred -> ch )
let co = 
    let cloneOptions = new CloneOptions()
    cloneOptions.CredentialsProvider <- credentials
    cloneOptions

let repoPath = 
    let discovery = Repository.Discover(path)
    if isNull discovery  then
        printfn "Loggin repository not found. Cloning ..."
        let clone = Repository.Clone(url, path, co)
        printfn "Cloning done!"
        clone
    else
        discovery

let createLogMessage path = sprintf "Log to %s" path

let commitApiLog (content:string list) (logApiPath: string) (repo0 : Repository) =
    using repo0 (fun repo ->
        let dir = repo.Info.WorkingDirectory
        let logFilePath = Path.Combine(dir, logApiPath) + ".txt"
        File.AppendAllLines(logFilePath, content)

        // stage file
        repo.Index.Add(logApiPath + ".txt")
        repo.Index.Write()


        // Create the committer's signature and commit
        let (author: Signature) = new Signature(userName, "@", System.DateTime.Now);

        let (commit: Commit) = repo.Commit(createLogMessage logApiPath, author, author)
        ()
    )

let gitPush (repo0 : Repository) =
    using repo0 (fun repo ->
        let options = new LibGit2Sharp.PushOptions()
        options.CredentialsProvider <- credentials
        repo.Network.Push(repo.Branches["main"], options)
    )

commitApiLog [@"Hello World!"] "log_api_test" <| new Repository(repoPath)

gitPush <| new Repository(repoPath)
