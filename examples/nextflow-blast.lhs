This example re-implements the example from https://www.nextflow.io/example3.html.
Unlike the simple example, we will use only the core BioShake framework, writing
stages for each part of processing.

The example is a simple blast example where a FastA file is sharded in a number
of chunks, blastp executed on each chunk against a database, and then the top
10 matches extracted from the blastp results. It is a typical scatter-gather
type of task.

> {-# LANGUAGE FlexibleContexts  #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TemplateHaskell   #-}
> {-# LANGUAGE TypeOperators     #-}
> {-# LANGUAGE ViewPatterns      #-}
> module Main where

We will use the core Bioshake definitions and template haskell functions
as well as some core Shake definitions:

> import Prelude hiding (concat)
> import Bioshake
> import Bioshake.TH
> import Development.Shake

For the FastA sharding, we will write a Haskell function. As BioShake is
an EDSL, we can use the full power of Haskell to do any processing we require.
In this case, we simply need to split strings based on a marker, which we
can get from the Split package and some basic IO functions.

> import Data.List.Split (splitOn)
> import System.Environment
> import System.FilePath
> import System.IO

We'll begin by defining a datatype to represent our FastA query sequences on
disk, i.e., the beginning of the workflow. We'll design our workflow to look for
files relative to a given prefix, so the type needs to hold the prefix:

> data Query = Query FilePath
>   deriving Show

The file is a FastA file, and as it already exists on disk we simply declare it
to be compilable.

> instance Compilable Query
> instance IsFastA Query

The location of the FastA file and Blast database is relative to the prefix held
by our Query object:

> instance Pathable Query where
>   paths (Query p) = [p </> "data/sample.fa"]

> instance Referenced Query where
>   getRef (Query p) = p </> "blast-db/pdb/tiny"
>   name _ = "PDB"

We now turn our attention to the first data processing stage, the sharding of
query sequences into a number of chunks. The `Chunk` datatype will represent
this sharding, and hold the number of chunks to create.

> data Chunk c = Chunk c Int
>   deriving Show

Each chunk will be a FastA file, and we name each chunk with a unique number
appended to a hash of the inputs.

> instance IsFastA (a :-> Chunk c)
> instance (Pathable a, Show a, Show c) => Pathable (a :-> Chunk c) where
>   paths (a :-> b@(Chunk _ n)) = [hashPath (paths a, show a, show b) <.> show i <.> "fasta" | i <- [1..n]]

Execution of the `Chunk` stage will split apart the input file into individual
sequences and allocate them to a chunk in a round-robin fashion.

> buildChunk (Chunk _ n) (paths -> [input]) outs =
>   liftIO $ do
>     seqs <- tail . splitOn ">" <$> readFile input
>     hs <- mapM (`openFile` WriteMode) outs
>     write seqs hs
>   where
>     write [] _ = return ()
>     write (s:ss) (h:hs) = do
>       hPutStr h ">"
>       hPutStr h s
>       write ss (hs ++ [h])

Template haskell can now be used to attach the build function to they type
representation. The input is required to be a FastA file.

> $(makeSingleThread ''Chunk [''IsFastA] 'buildChunk)

The next stage is to blast the query sequences against the database. Blast's
output format has no representation in core Bioshake, hence we create a
typeclass to represent the type of output from Blast. A type to represent
blasting is also defined and we declare its output type.

> class IsHits x
> data Blast c = Blast c
>   deriving Show
> $(makeSingleTypes ''Blast [''IsHits] [])

We define a function to invoke blastp and declare that its input must be a FastA
file that is associated with some reference database.

> buildBlast _ i@(paths -> [input]) [out] =
>   run "blastp"
>     ["-db", getRef i]
>     ["-query", input]
>     "-outfmt 6"
>     [">", out]
> $(makeSingleThread ''Blast [''IsFastA, ''Referenced] 'buildBlast)

Next we extract the top 10 sequences. Again, a typeclass is used to track the
new file format and a type represents the stage itself.

> class IsSeqs x
> instance IsSeqs x => IsSeqs (All x)
> data Extract c = Extract c
>   deriving Show
> $(makeSingleTypes ''Extract [''IsSeqs] [])

The stage simply invokes blast db query function and limits it to the first 10
lines:

> buildExtract _ i@(paths -> [input]) [out] =
>   run "blastdbcmd"
>     ["-db", getRef i]
>     ["-entry_batch", input]
>     "| head -n 10 >"
>     [out]
> $(makeSingleThread ''Extract [''IsHits, ''Referenced] 'buildExtract)

The final stage is to concat all the sharded sequences to a final output file.
This stage accepts sequences, produces sequences, and is executed by using the
standard concatenation unix facilities:

> data Concat c = Concat c
>   deriving Show
> $(makeSingleTypes ''Concat [''IsSeqs] [])

> buildConcat _ (paths -> inputs) [out] =
>   run "cat" inputs ">" [out]
> $(makeSingleThread ''Concat [''IsSeqs] 'buildConcat)

Unlike the original example, we will not assume a given path nor shard size and
read them from the command line arguments. For simplicity, we will not parse
arguments carefully and mostly assume the arguments are well formed. We will
also assume a maximum of 10 concurrent threads.

> main = do
>   [path, read -> n] <- getArgs
>   withArgs [] $ bioshake 10 shakeOptions $ compileRules $

The pipeline definition, given our stages defined above, is straight forward. We
first pipe our query sequence file into our sharding stage (chunk) and then
split the outputs into individual chunks. Over each chunk, we blast the query
and extract the top hits. We then combine all individual files and concatenate
them, saving the output as a named text file.

>     let chunks = split (Query path :-> chunk n)
>     in compileAndWant $ withAll (flip map chunks $ \c -> c :-> blast :-> extract) :-> concat :-> out ["seqs.txt"]
