This is a simple workflow to demonstrate how to specify workflows using the
bioshake framework. It will accept pairs of fastq files from the command line,
align them against a reference sequence with BWA, then call variants on all
asamples using the platypus variant caller.

> import Bioshake
> import Control.Monad
> import Data.List.Split
> import Development.Shake
> import Development.Shake.FilePath
> import System.Environment
> import System.Exit

We will align reads using BWA, sort and filter with samtools, and finally call
with Platypus

> import Bioshake.BWA as B
> import Bioshake.Platypus
> import Bioshake.Samtools as S

Note that if we wanted to submit our jobs to a cluster instead of executing
directly, we would import the cluster versions of the modules, e.g.,:

> --import Bioshake.Cluster.BWA as B
> --import Bioshake.Cluster.Platypus
> --import Bioshake.Cluster.Samtools as S


First, define a datatype to represent our paired-end samples.

> data Sample = Sample FilePath FilePath
> instance Show Sample where
>   show (Sample a b) = takeFileName (dropExtensions a) ++ "-" ++ takeFileName (dropExtensions b)

The default instance of Compilabe suffices for files that already exist on disk
and that do not require building

> instance Compilable Sample

Bioshake uses type classes to encode properties about stages in the workflow.
The first property we're going to declare is that these samples are paired end
reads. We also will declare that the input files are FastQ files.

> instance PairedEnd Sample
> instance IsFastQ Sample

Next, we need to declare which reference the type is going to be associated
with. This involves instantiating the Referenced class and declaring the path to
wthe reference and the short name of the reference:

> instance Referenced Sample where
>   getRef _ = "ref.fa"
>   name _ = "SL1344"

Finally, we describe how to get the paths for a Sample, which in this case is
extracted from our Sample datatype:

> instance Pathable Sample where
>   paths (Sample a b) = [a, b]

Command line arguments are naïvly parsed: we simply assume each pair of
arguments are paths to the paired-end reads.

> parseArgs = map (\[a, b] -> Sample a b) . chunksOf 2
> main = do
>   args <- getArgs
>   when (null args || length args `mod` 2 /= 0) $ do
>     putStrLn "error: expecting paired fastq files as input"
>     exitFailure
>   let samples = parseArgs args

The number of threads used has to be specified to bioshake in two ways: the
number of threads used for each stage, and the maximum number of concurrent
threads in total. Threads per job can be specified by giving a Threads instance
to each stage, or at a higher level. Here we give Threads 1, meaning stages run
single threaded, and limit the maximum number of concurrent threads to 2 in total.

>   withArgs [] $ give (Threads 1) $ bioshake 2 shakeOptions $ do

bioshake, like shakeArgs, expects Shake Rules. We can therefore want thing and
define standard Shake Rules as normal. In this case we want our output vcf file,
which we'll call "calls.vcf".

>     want ["calls.vcf"]

In addition to that, we will bring into scope the rules for indexing bamfiles
(building .bam.bai from .bam) using samtools.

>     B.indexRules
>     S.indexRules

Finally, we compile our workflow down to Shake Rules.

>     compileRules $

We have one simple workflow in this case. Alignment and processing is first
applied to each individual sample. The samples are then pooled and called as a
group using platypus.

>       let aligned = map (\s -> s :-> align
>                                  :-> fixMates
>                                  :-> sortBam
>                                  :-> markDups
>                                  :-> addRGLine (show s)) samples in
>       compile $ withAll aligned :-> call :-> out ["calls.vcf"]
