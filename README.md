# High Quality Hypergraph Partitioning via Max-Flow-Min-Cut Computations

**Abstract**
Currently, algorithms based on the *FM* idea are the 
only practical heuristics to improve a k-way
partition in a multilevel hypergraph partitioner. However, they are often criticized for their 
limited ability to lookahead. It might be more beneficial to move a 
hypernode with small gain, because it will induce many good moves later.
We present an alternative *local search* approach based on *Max-Flow-Min-Cut*
computations. The framework is inspired by the work of Sanders and Schulz who
successfully showed that *flow*-based refinement in combination with the *FM*
algorithm significantly improve the quality of partitions in a multilevel graph partitioner.
In this work, we develop different modeling techniques of a hypergraph as flow network and
show how to improves the connectivity metric of a k-way partition by building a flow problem 
on a subset of the vertices. We integrated the framework in the hypergraph partitioner 
*KaHyPar* by applying and adapting the basic framework of Sanders and Schulz.
On our large benchmark set with 3222 instances our new configuration outperforms all 
state-of-the-art hypergraph partitioner on 70\% of the instances. In comparison to the latest
configuration of *KaHyPar* our new approach produces 2\% better quality by a performance
slowdown only by a factor of 2.

## Illustration of our Flow-Based Refinement Framework
![Max-Flow-Min-Cut Hypergraph Partitioning Framework](/img/flow_local_search/flow_framework_hypergraph.png)

## Comparison with other Hypergraph Partitioner
Our full benchmark set consists of 488 hypergraphs. We choose our benchmarks 
from three different research areas. For VLSI design we use instances from
the *ISPD98 VLSI Circuit Benchmark Suite* and add more recent instances of the *DAC 2012 Routability-Driven Placement Contest*. Further, we interpret the Sparse Matrix instances of the *Florida Sparse Matrix 
collection* as hypergraphs using the row-net model. Our last benchmark type are SAT formulas of the *International SAT Competition 2014*.
We compare our flow-based framework KaHyPar-MF against the state-of-the-art hypergraph partitioner *hMetis* and *PaToH*.
*hMetis* provides a direct k-way (hMetis-K) and recursive bisection (hMetis-R) implementation. Further, we also use the default configuration (PaToH-D) and quality preset (PaToH-Q) of *PaToH*. Additionally, we use the latest configuration with *(C)ommunity-(A)ware* coarsening of *KaHyPar* (KaHyPar-CA).
For an explanation of the plot below, we refer the reader to [KaHyPar]( http://kahypar.org).

![Comparision with other Hypergraph Partitioner](/img/final_result.png)