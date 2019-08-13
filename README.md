# Bioshake: Bioinformatics pipelining with Shake

Bioshake is a bioinformatics workflow tool extending the Shake build system.

Pipelines are defined in terms of
_stages_ that connect together into simple streams. Here's an example pipeline:

    Input :-> align :-> mappedOnly :-> sortBam :-> deDup :-> out ["aligned.bam"]

Each stage takes its input from the previous stage and generates
output for the stage that follows. Some features of Bioshake pipelines are

-   There is no need to mention files between
    the stages: intermediate files are automatically named. The `out` stage 
    simply copies its input to explicitly named files. 
-   Each stage has strong type guarantees. For example, if a stage
    requires a sorted BAM file, it will be a type error to construct a pipeline that
    feeds it an unsorted BAM file. This allows many pipeline errors to be caught at
    compile time rather than runtime.
    
## Quickstart

This repository includes [Nix](http://nixos.org/nix) expressions for easy
building. To use, install Nix, clone this repository, then a shell containing
the necessary GHC environment for building Bioshake can be dropped into with
"nix-shell". To compile and execute the example pipeline, the following commands
can be used:

 
    nix-shell --command 'ghc -o examples/simple examples/simple.lhs -O'
    cd examples
    nix-shell -p bwa samtools platypus --command "./simple *.fq"

Note that the repository must be cloned with `git lfs` installed for
the example data to be downloaded, see: [examples/README](examples/README).
## How to use

Begin with something like this:

    main = bioshake threads shakeOptions $ do ...
    
This initialises bioshake with a certain number of threads and with some Shake
options. 

The (simplified) type signature of the `bioshake` function  is

    bioshake :: Int -> ShakeOptions -> Rules () -> IO ()
    
and so the `do` clause corresponds to a Shake `Rules ()` object; 
Bioshake pipelines are compiled down to Shake `Rules ()` and so can be partially
interleaved with them. (In Shake, a `Rules a` object is a (collection of) build rules that return data of type `a`.) 

Within the `Rules ()` block, a pipeline looks like

      compileRules $ do
        compile $ 
          Input :-> align :-> mappedOnly :-> sortBam :-> deDup :-> out ["aligned.bam"]
          
        ...

The `compileRules` and `compile` functions have the (again simplified) types

    compileRules :: Compiler () -> Rules ()
    compile :: Compilable a => a -> Compiler () 

So `compile` takes `Compilable` things (the pipeline in this case) and
makes something suitable for `compileRules`, which generates the actual shake
`Rules`.

A Bioshake pipeline must instantiate `Compilable`, but pipelines 
(and stages) are not themselves `Compiler`s. Unlike vanilla Shake -- in which
each stage of a build is represented by an object of type `Rules a` -- 
in Bioshake, there is no concrete datatype representing a stage; rather, 
each stage is the only inhabitant of its type, and the whole functionality 
of a stage is determined by the typeclasses that this type instantiates.
More on this below.

### Direct execution

In our pipeline, the stages `align`, `mappedOnly`, `sortBam`, and
`deDup` come from different modules such as `Bioshake.BWA` and
`Bioshake.Samtools`, or their cluster equivalents `Bioshake.Cluster.BWA` and
`Bioshake.Cluster.Samtools`. 
Each module in bioshake represents a tool, such as an aligner; for example, 
`Bioshake.BWA` is the module that defines alignment stages using the BWA aligner.
When a pipeline is executed using this module, the tool is directly invoked on
the machine that is executing bioshake. Many of these tools are multithreaded,
and thus you need to declare how many threads to use by default:

    instance Default Threads where def = Threads threads

This is overridable for specific stages using the `$~` operator. For example, if
we wanted to restrict the above pipeline to only 5 threads during alignment we
could write:

    Input :-> align $~ Threads 5 :-> mappedOnly :-> sortBam :-> deDup :-> out ["aligned.bam"]
    
Note that bioshake will not overcommit resources: the first argument to
`bioshake` defines the maximum number of concurrent threads at any given time.

### Cluster submission

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

### Writing your own stages

As mentioned above, in ordinary Shake, each stage of a build is represented by an object of
type `Rules a`. By contrast, in Bioshake, each stage inhabits its own type, and
the functionality of a stage is determined by the typeclasses 
that its type instantiates. Many of these instances can be automatically generated
by Template Haskell. The constructor `:->`, which is used to concatenate successive
stages into a pipeline, is isomorphic to `(,)`.

Here is an example showing how to create your own stage. We will
create a stage called `Magic` that takes a BAM file as input and
produces a VCF as output.

First, create a datatype to represent the stage:

    data Magic c = Magic c deriving Show
    
The `c` type variable is a configuration type which is used by bioshake to represent
threaded/cluster configuration. You can have other polymorphic types, but the
last type must be the configuration type. Next, write a function that executes
the tool:

    buildMagic (Magic c) (paths -> [input]) [out] =
      run "/some/tool" ["-a", "with-flags"] [input] [out]
  
Here `run` is pretty much like a restricted version of shake's `cmd`. Now use
template haskell to do the rest:

    $(makeSingleTypes ''Magic [''IsVCF] [])
    $(makeSingleThread ''Magic [''IsBAM] 'buildMagic)

The `makeSingleTypes` macro declares that `Magic` maps each input file to an output file (in
this case `buildMagic` expects a single input and maps it to a single output).
The first list in the template function declares properties about the _output_
of the stage: the first must be a `IsEXT` tag, which declares the type of output
and the extension. The second list is a bunch of transitive tags: if this tags
hold for the input, then they will hold for the output too.

`makeSingleThread` declares how to build `Magic` things. The list is a bunch of
tags that must hold for the _input_ files, and the second parameter is just the
function to build output files given inputs.

### The Input stage

The first stage `input` is probably the most confusing part about getting a
pipeline running. Stages generate some output that is fed to the next stage in
the pipeline, hence the first stage has to "generate" some output files, which
are just the input files on disk. We need some data structure to represent input
files on the disk:

    data Input = Input deriving Show
    
First is to declare the paths "output" from the stage. As the input stage, we
just return a list of input files:

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

## Further documentation

There is an example pipeline in the repository and haddock documentation
at <https://papenfusslab.github.io/bioshake/>.
