# Bioshake: Bioinformatics pipelining with Shake

Bioshake is a layer on top of Shake that provides explicit dependency
definitions with wrappers for common tools. Pipelines are defined in terms of
_stage_ that connect together into simple streams. Here's an example pipeline:

    Input :-> align :-> mappedOnly :-> sortBam :-> deDup :-> out ["aligned.bam"]
  
In the above each phase takes output from the previous stage and generates
output for the subsequent phase. Note that there are no mention of files between
the stages: intermediate files are automatically named. The `out` phase is a
special phase that simply copies it's input to explicitly named files. This
allows the outputs that are of interest to be specifically named.

Furthermore, each phase has strong type guarantees. For example, if a stage
requires a sorted BAM file, it will be a type error to construct a pipeline that
feeds it an unsorted BAM file. This allows many pipeline errors to be caught at
compile time rather than runtime.

## How to use

Do something like this:

    main = bioshake threads shakeOptions $ do ...
    
This initialises bioshake with a certain number of threads and with some shake
options. The (simplified) type signature of bioshake is

    bioshake :: Int -> ShakeOptions -> Rules () -> IO ()
    
hence it expects a bunch of shake `Rules`. You can just define shake rules as
normal, but we're interested in pipelines, hence we continue the example:

      compileRules $ do
        compile $ 
          Input :-> align :-> mappedOnly :-> sortBam :-> deDup :-> out ["aligned.bam"]
          
        ...

`compileRules` and `compile` have the (again simplified) types

    compileRules :: Compiler () -> Rules ()
    compile :: Compilable a => a -> Compiler () 

`compile` therefore takes `Compilable` things (the pipeline in this case) and
makes something suitable for `compileRules`, which generates the actual shake
`Rules`. In our pipeline, the stages `align`, `mappedOnly`, `sortBam`, and
`deDup` come from different modules such as `Bioshake.BWA` and
`Bioshake.Samtools`, or their cluster equivalents `Bioshake.Cluster.BWA` and
`Bioshake.Cluster.Samtools`. More on these modules later

The first stage `input` is probably the most confusing part about getting a
pipeline running. Stages generate some output that is fed to the next stage in
the pipeline, hence the first stage has to "generate" some output files, which
are just the input files on disk. We need some data structure to represent input
files on the disk:

    data Input = Input deriving Show
    
First this is to declare the paths "output" from the stage. As the input stage,
we just return a list of input files:

    instance Pathable Input where
      paths _ = ["sample_R1.fastq.gz", "sample_R2.fastq.gz"]

We've assumed the inputs here are paired end reads, hence there are two fastq
files. This is not sufficient as we must declare our inputs have certain
properties: it is compilable to shake rules, that the reads are from paired end
sequencing, that they are fastq files, and furthermore that they should be
referenced against some genome:
    
    instance Compilable Input
    instance PairedEnd Input
    instance IsFastQ Input
    instance Referenced In where
      getRef _ = "/path/to/hg38.fa"
      name _ = "hg38"
    
## Direct execution 

Each module in bioshake represents a tool, such as an aligner. `Bioshake.BWA`
for example is the module that defines alignment stages using the BWA aligner.
When a pipeline is executed using this module, the tool is directly invoked on
the machine that's executing bioshake. Many of these tools are multithreaded,
and thus you need to declare how many threads to use by default:

    instance Default Threads where def = Threads threads

This is overridable for specific stages using the `$~` operator. For example, if
we wanted to restrict the above pipeline to only 5 threads during alignment we
could write:

    Input :-> align $~ Threads 5 :-> mappedOnly :-> sortBam :-> deDup :-> out ["aligned.bam"]
    
Note that bioshake will not overcommit resources: the first argument to
`bioshake` defines the maximum number of concurrent threads at any given time.

## Cluster submission

Stages can be submitted to a cluster instead of executing directly. In this
case, the `Bioshake.Cluster.*` modules are used. Continuing the BWA example, to
submit BWA jobs to a cluster instead of executing directly you would import
`Bioshake.Cluster.BWA`. In this case, the default cluster configuration must be
defined:

    instance Default Config where def = Config [Queue "somequeue", Mem (gb 20),
    CPUs 42]
    
There are a several configuration options that can be defined:

1. `Mem`: the maximum memory used by the stage
2. `CPUs`: the maximum number of CPUs
3. `Walltime`: maximum walltime allowed
4. `Queue`: the queue to submit to
5. `Module`: a module to load before execution

## Writing your own stages

First create a datatype to represent your stage:

    data Magic c = Magic c deriving Show
    
The `c` type here is a configuration type which is used by bioshake to represent
threaded/cluster configuration. You can have other polymorphic types, but the
last type must be the configuration type. Next, write a function that executes
the tool:

    buildMagic (Magic c) (paths -> [input]) [out] =
      run "/some/tool" ["-a", "with-flags"] [input] [out]
  
`run` here is pretty much like a restricted version of shake's `cmd`. Now use
template haskell to do the rest:

    $(makeSingleTypes ''Magic [''IsVCF] [])
    $(makeSingleThread ''Magic [''IsBAM] 'buildMagic)

`makeSingleTypes` declares `Magic` maps each input file to an output file (in
this case `buildMagic` expects a single input and maps it to a single output).
The first list in the template function declares properties about the _output_
of the stage: the first must be a `IsEXT` tag, which declares the type of output
and the extension. The second list is a bunch of transitive tags: if this tags
hold for the input, then they will hold for the output too.

`makeSingleThread` declares how to build `Magic` things. The list is a bunch of
tags that must hold for the _input_ files, and the second parameter is just the
function to build output files given inputs.
