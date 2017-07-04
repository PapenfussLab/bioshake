This is a very simple pipeline to demonstrate how to specify pipelines using the
bioshake framework. It will accept pairs of fastq files from the command line,
align them against a reference sequence with BWA, then call variants on all
asamples using the platypus variant caller.

First, import some stuff...

> import Bioshake
> import Bioshake.BWA
> import Bioshake.Platypus
> import Control.Monad
> import Data.List.Split
> import Development.Shake
> import Development.Shake.FilePath
> import System.Environment

We will align reads using BWA, sort and filter with samtools, and finally call
with Platypus

> import Bioshake.BWA
> import Bioshake.Platypus
> import Bioshake.Samtools

First, define a datatype to represent our paired-end samples.

> data Sample = Sample FilePath FilePath
> instance Show Sample where
>   show (Sample a b) = takeFileName (dropExtensions a) ++ "-" ++ takeFileName (dropExtensions b)
> instance Compilable Sample

Bioshake uses types to encode properties about stages in the pipeline. The first
property we're going to declare is that these samples are paired end samples. We
palso will declare that the input files are FastQ files.

> instance PairedEnd Sample
> instance IsFastQ Sample

Next, we need to declare which reference the type is going to be associated
with. This involves instantiating the Referenced class and declaring the path to
wthe reference and the short name of the reference:

> instance Referenced Sample where
>   getRef _ = "/data/hg38/hg38.fa"
>   name _ = "hg38"

Finally, we describe how to get the paths for a Sample:

> instance Pathable Sample where
>   paths (Sample a b) = [a, b]

Bioshake support multithreaded jobs, but for the purposes of this simple example
we'll just run jobs in a single thread.

> instance Default Threads where
>   def = Threads 1

Command line arguments are naïvly parsed: we simply assume each pair of
arguments are paths to the paired-end reads.

> parseArgs = getArgs >>= return . map (\[a, b] -> Sample a b) . chunksOf 2

> main = do
>   samples <- parseArgs

As we have parsed commandline arguments already, we clear them out before
calling bioshake. Shake will parse the command line arguments and alter the
targets it build otherwise. Note that the maximum number of threads allowed has
to be specified to bioshake; above we defined the number of threads for
individual programs to use by default, whereas this threads parameter controls
the maximum allowed at any one time and so must at least exceed the maximum
number of threads used by a single job.

>   withArgs [] $ bioshake 1 shakeOptions $ do

bioshake, like shakeArgs, expects Shake Rules. We can therefore want thing and
define standard Shake Rules as normal. In this case we want our output vcf file,
which we'll call "calls.vcf".

>     want ["calls.vcf"]

In addition to that, we will bring into scope the rules for indexing bamfiles
(building .bam.bai from .bam) using samtools.

>     indexRules

Finally, we compile our pipeline down to Shake Rules.

>     compileRules $

We have one simple pipelines in this case. Simple alignment and processing is
first applied to each individual sample. The samples are then pooled and called
as a group using platypus.

>       let aligned = map (\s -> s :-> align :-> mappedOnly :-> sortBam :-> deDup :-> addRGLine (show s)) samples in
>       compile $ withAll aligned :-> call :-> out ["calls.vcf"]

