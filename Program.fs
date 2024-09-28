open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Argu
open QuikGraph
open QuikGraph.Algorithms
open System.Text.Json.Serialization

type NodeString = NodeString of string

type Namespace = private Namespace of string
with
  static member get (Namespace ns) = ns
  static member dotFormatNamespace (Namespace ns) =
    ns.Replace(".", "_")

  static member dotNode comment ns =
    let n = Namespace.dotFormatNamespace ns
    let (Namespace ns) = ns
    NodeString $"%s{n} [label = \"%s{ns}\"] # %s{comment}"

type FileNamespace = {
  Filename: string
  Namespace: Namespace
}
with
  static member toString fn =
    let (Namespace ns) = fn.Namespace
    $"%s{ns} %s{fn.Filename}"
  static member toDot fn =
    let (Namespace ns) = fn.Namespace
    $"%s{ns} %s{fn.Filename}"
  static member getNamespace fn = fn.Namespace
  static member toDotNode comment fn =
    Namespace.dotNode comment fn.Namespace

type FileUsings = {
  Filename: string
  Usings: Namespace[]
}
with
  static member toDot fu =
    let us = fu.Usings |> Seq.map Namespace.get |> String.concat ", "
    $"%s{fu.Filename} %s{us}"

type NamespaceDependencies = {
  Namespace: Namespace
  Dependencies: Namespace[]
}
with
  static member toDot isConnected nsdeps = seq {
    let ns = Namespace.dotFormatNamespace nsdeps.Namespace
    let write i dep =
      let depNs = Namespace.dotFormatNamespace dep
      $"%s{ns} -> %s{depNs} # %d{i}"
    let res =
      nsdeps.Dependencies
      |> Seq.where isConnected
      |> Seq.mapi write
      |> String.concat Environment.NewLine
    $"# %s{ns}"
    res
  }

module Parse =
  let private namespaceRegex = Regex("^namespace (?<Namespace>.*?)(;| *{)?$")
  let private isMatch filename line =
    let matcher = namespaceRegex.Match line
    if matcher.Success then
      let ns = matcher.Groups["Namespace"].Value
      Some { Filename = filename; Namespace = Namespace ns }
    else None
  let getFileNamespace filename lines =
    let ns =
      lines
      |> Array.choose (isMatch filename)
    if Array.length ns = 0 then [| { Filename = filename; Namespace = Namespace "global" } |]
    else ns

  let private usingsRegex = Regex("^using (static )?(\w+ = |System.*|Microsoft.*)?(?<Namespace>.*?);$")
  let private isUsing included line =
    let matcher = usingsRegex.Match line
    if matcher.Success then
      let ns = matcher.Groups["Namespace"].Value
      if String.IsNullOrEmpty(ns) || not (Set.contains ns included) then
        None
      else Some ns
    else None
  let getFileUsings filename included lines =
    let usings = lines |> Array.choose (isUsing included) |> Array.map Namespace
    { Filename = filename; Usings = usings }

module Out =
  let private getLineWriter (writer: StreamWriter) =
    fun (l: string) ->
      if l <> l.TrimEnd() then failwith l
      writer.WriteLine l
  let writeDeps fileNamespaces (namespaceDependencies: NamespaceDependencies[]) (components: Set<string>[]) onlyConnected filename =
    let isConnected =
      let total = Seq.collect id components |> set
      fun s l ->
        let form = Namespace.dotFormatNamespace l
        let f =
          if not onlyConnected then true
          else Set.contains form total
        f
    use file = File.Create filename
    use writer = new StreamWriter(file)
    let w = getLineWriter writer
    let usingNamespaces =
      namespaceDependencies
      |> Seq.map (fun nds -> nds.Dependencies)
      |> Seq.collect id
      |> Seq.where (isConnected "1")
      |> Seq.map (Namespace.dotNode "usingNamespaces")
    let outputFileWithoutExtension = IO.Path.GetFileNameWithoutExtension filename
    w $"@startuml %s{outputFileWithoutExtension}"
    let now = DateTime.Now.ToString("o")
    w $"' generated at %s{now}"
    w "digraph G {"
    w "# components"
    let componentGroups =
      components
      |> Seq.mapi (fun i v -> i, Set.count v)
      |> dict
    components
    |> Seq.map (fun nss ->
      nss |> Seq.map (fun s -> $"  %s{s} [color = blue]") |> String.concat Environment.NewLine)
    |> Seq.iteri (fun i ls ->
      w $"subgraph cluster_%d{i} {{"
      w $"  label=\"Component with %d{componentGroups[i]} parts\""
      w $"  labelloc=\"t\""
      w $"%s{ls}"
      w "}")
    w "# fileNamespaces and usingNamespaces"
    fileNamespaces
    |> Seq.where (fun (ns: FileNamespace) -> isConnected "2" ns.Namespace)
    |> Seq.map (FileNamespace.toDotNode "fileNamespaces")
    |> Seq.append usingNamespaces
    |> Seq.distinct
    |> Seq.sort
    |> Seq.iter (fun (NodeString ns) -> w ns)

    w "# dependencies"
    namespaceDependencies
    |> Seq.where (fun ns -> isConnected "3" ns.Namespace)
    |> Seq.map (NamespaceDependencies.toDot (isConnected "4"))
    |> Seq.collect id
    |> Seq.iter w
    w "}"
    w "@enduml"

type Arguments =
  | [<Mandatory>] Source_Dir of string
  | [<Mandatory>] Output_File of string
  | Only_Connected
  | Debug
  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Source_Dir _ -> "Directory of source files to scan"
      | Output_File _ -> "File to write analysis to"
      | Only_Connected -> "Only output connected components"
      | Debug -> "Enables debug mode"

let opts = Text.Json.JsonSerializerOptions(WriteIndented = true)
JsonFSharpOptions()
    .WithUnionUnwrapFieldlessTags()
    .AddToJsonSerializerOptions(opts)

type DebugFunction = String -> Object -> unit
let makeD debug =
  fun title ob -> if debug then printfn "%s %s" title (Text.Json.JsonSerializer.Serialize(ob, opts))

let doRun sourceDir outputFile onlyConnected (d: DebugFunction) =
  let files = Directory.GetFiles(sourceDir, "*.cs", SearchOption.AllDirectories) |> Array.where (fun f -> not <| f.Contains(@"obj\"))

  // figure out the namespace of the files
  let namespaces =
    files
    |> Array.map (fun f -> File.ReadAllLines f |> Parse.getFileNamespace f)
    |> Array.collect id

  let included = namespaces |> Seq.map (fun ns -> Namespace.get ns.Namespace) |> set
  let fileUsings =
    files
    |> Array.map (fun f -> File.ReadAllLines f |> Parse.getFileUsings f included)

  d "** fileUsings" fileUsings
  d "** namespaces" namespaces
  let nsDict = namespaces |> Seq.map (fun ns -> ns.Filename, ns.Namespace) |> dict
  d "** nsDict" nsDict
  let usingsDict = fileUsings |> Seq.map (fun fu -> fu.Filename, fu.Usings) |> dict
  d "** usingsDict" usingsDict
  let deps =
    usingsDict
    |> Seq.map (fun kvp -> let b, s = nsDict.TryGetValue(kvp.Key) in if not b then failwithf "failed to find %s" kvp.Key else s, kvp.Value)
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> k, v |> Seq.map snd)
    |> Seq.map (fun (k, v) -> {
      Namespace = k
      Dependencies =
        Seq.collect id v
        |> Seq.distinct
        |> Seq.sort
        |> Array.ofSeq })
    |> Array.ofSeq

  let edges =
    deps
    |> Array.map (fun d -> Array.map (fun x -> SEdge<_>(Namespace.dotFormatNamespace x, Namespace.dotFormatNamespace d.Namespace)) d.Dependencies)
    |> Array.collect id

  let graph = edges.ToAdjacencyGraph()
  let componentDict = Dictionary<string,int>()
  let _ = graph.StronglyConnectedComponents(componentDict)

  let components =
    componentDict
    |> Seq.groupBy (fun kvp -> kvp.Value)
    |> Seq.map (fun (_, seq) ->
      seq
      |> Seq.map (fun kvp -> kvp.Key)
      |> Set.ofSeq)
    |> Seq.where (fun s -> Set.count s > 1)
    |> Array.ofSeq

  d "** components" components
  Out.writeDeps namespaces deps components onlyConnected outputFile
  0

[<EntryPoint>]
let main args =

  try
    let parser = ArgumentParser.Create<Arguments>("ConsoleF")
    let results = parser.ParseCommandLine(args)
    let sourceDir = results.GetResult Source_Dir
    let outputFile = results.GetResult Output_File
    let onlyConnected = results.Contains Only_Connected
    let debug = results.Contains Debug |> makeD
    doRun sourceDir outputFile onlyConnected debug
  with
  | :? Argu.ArguParseException as e ->
    eprintfn "%s" e.Message
    1
