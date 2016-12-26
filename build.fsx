// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing.Expecto

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"

let testDir = "./build.test/"

// Filesets
let appReferences  =
    !! "src/**/*.csproj"
    ++ "src/**/*.fsproj"

let testReferences = 
    !! "test/**/*.fsproj"


// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    MSBuildDebug testDir "Build" testReferences
        |> Log "TestBuild-Output: "    
)

let testExecutables = !! (testDir + "/Test.*.exe")

Target "expectoTest" (fun _ ->
    testExecutables
    |> Expecto (fun p ->
        { p with
            Debug = true
            Parallel = true
            // use only one of the following parameters
            Run = ["Insert Site"]
            ListTests = false
        })
)


Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

// Build order
"Clean"
  ==> "Build"
  ==> "BuildTest"
  ==> "expectoTest"
  ==> "Deploy"

// start build
RunTargetOrDefault "Build"
