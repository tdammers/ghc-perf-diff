# ghc-perf-diff

A little tool to compare perf-test output between GHC versions, similar to
nofib-analyse.

## Introduction

This little tool can parse the output from one or two runs of GHC's perf test
suite, and output a concise report similar to the output of nofib-analyse. This
can be useful when you are developing features in GHC, and you want to get some
finer-grained information about performance deviations you introduce than what
the testsuite can give you on its own.

## Installing

Use cabal new-build / new-install; stack might also work though I haven't
tested it.

## Usage

1. Check out a "baseline" version of GHC that you want to compare against.
   Build it, and then run the perf tests into a file:

   ```sh
   $ cd testsuite/tests/perf; make VERBOSE=4 > ../../../logs/baseline.log
   ```

2. Now check out and build the patched version you want to compare, and do the
   same dance again:

   ```sh
   $ cd testsuite/tests/perf; make VERBOSE=4 > ../../../logs/patched.log
   ```

3. Run `ghc-perf-diff` to compare the two versions:

   ```sh
   $ cd logs; ghc-perf-diff baseline.log patched.log
   ```

This should produce output like the following:

```
test                   type               baseline.log     patched.log      dev.
--------------------------------------------------------------------------------
ManyAlter...es(normal) bytes alloc          1398185168      1396695520     -0.1%
ManyConst...rs(normal) bytes alloc          4211332168      4200639112     -0.3%
MultiLaye...es(normal) bytes alloc          5969305304      6000869392     +0.5%
Naperian(optasm)       bytes alloc            56632624        56616888     -0.0%
T10370(optasm)         peak MB alloc               106             107     +0.9%
T10370(optasm)         max bytes used         30454032        28921608     -5.0%
T10547(normal)         bytes alloc            38650560        38662016     +0.0%
T12150(optasm)         bytes alloc            76574104        76394064     -0.2%
T12227(normal)         bytes alloc           745748232       744873984     -0.1%
T12234(optasm)         bytes alloc            84829064        84677256     -0.2%
T12425(optasm)         bytes alloc           141814776       141804024     -0.0%
T12545(normal)         bytes alloc          3200696104      3212031504     +0.4%
T12707(normal)         bytes alloc          1157928672      1154676744     -0.3%
T13035(normal)         bytes alloc           123740664       123609928     -0.1%
T13056(optasm)         bytes alloc           415808568       415012856     -0.2%
T13379(normal)         bytes alloc           421145528       419576024     -0.4%
T13701(normal)         bytes alloc          2483820752      2483246784     -0.0%
T13719(normal)         bytes alloc          4896394968      4896700520     +0.0%
T14683(normal)         bytes alloc         10571217864     10579911448     +0.1%
T14697(normal)         bytes alloc           364670176       364277392     -0.1%
T15164(normal)         bytes alloc          1948045800      1947950888     -0.0%
T1969(normal)          bytes alloc           676330952       674573456     -0.3%
T1969(normal)          peak MB alloc                71              70     -1.4%
T1969(normal)          max bytes used         19208344        19131824     -0.4%
T3064(normal)          bytes alloc           272007792       269923632     -0.8%
T3064(normal)          peak MB alloc                64              64     +0.0%
T3294(normal)          bytes alloc          1865009256      1864726048     -0.0%
T3294(normal)          max bytes used         36514992        36807512     +0.8%
T4801(normal)          bytes alloc           383105784       383294928     +0.0%
T5030(normal)          bytes alloc           757211816       757037432     -0.0%
T5321FD(normal)        bytes alloc           361924880       361041128     -0.2%
T5321Fun(normal)       bytes alloc           439790152       438838640     -0.2%
T5631(normal)          bytes alloc          1168105336      1165383416     -0.2%
T5642(normal)          bytes alloc           791871800       791675224     -0.0%
T5837(normal)          bytes alloc            53314320        53313560     -0.0%
T6048(optasm)          bytes alloc            97167192        97100856     -0.1%
T783(normal)           bytes alloc           455299264       454485944     -0.2%
T9020(optasm)          bytes alloc           402952512       403392208     +0.1%
T9233(normal)          bytes alloc           986193304       968180848     -1.8%
T9630(normal)          max bytes used         40459264        38599992     -4.6%
T9675(optasm)          bytes alloc           675946368       675560832     -0.1%
T9675(optasm)          peak MB alloc                73              66     -9.6%
T9675(optasm)          max bytes used         19791816        17882896     -9.6%
T9872a(normal)         bytes alloc          2813952968      2811387592     -0.1%
T9872b(normal)         bytes alloc          3879635024      3874401360     -0.1%
T9872c(normal)         bytes alloc          3194525648      3197862632     +0.1%
T9872d(normal)         bytes alloc           602667312       602524640     -0.0%
T9961(normal)          bytes alloc           503696840       503629296     -0.0%
parsing001(normal)     bytes alloc           508522832       508524448     +0.0%
```

**Note** the following peculiarities:

- Tests that only appear in one of both outputs will be ignored
- By default, the GHC testsuite only reports metrics on tests that exceed the
  deviation limit; use `VERBOSE=4` to include all the metrics in the output
