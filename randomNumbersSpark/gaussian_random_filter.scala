/*
     Generating Gaussian numbers : Performance with repartition
     RDD partition size retains from the parent
     
     (c) 2017 
     GPLv3
     
     Author: Mehmet Suzen (suzen at acm dot org)

     Run this script on spark-shell
     spark-shell --executor-cores 4
     spark-shell> :load gaussian_random_filter.scala

 */

import util.Random     // normal
import breeze.linalg._ // Linear Algebra objects and csvwrite
import java.io.File    // File io

/*
 
   Generate gaussian random numbers manually without mllib
   Benchmark repartition

 */
// Random numbers to generate 
val Ns = Array(1e3, 1e4, 5e4, 1e5, 5e5, 1e6, 2e6, 4e6, 8e6, 1e7)
val benchMat = DenseMatrix.zeros[Double](10,3) 
Random.setSeed(4242)
for(i <- 0 to Ns.size-1) {
   println("running for " + Ns(i))
   // Generate random RDD size Ns
   var ngauss     = (1 to Ns(i).toInt).map(x=>Random.nextGaussian) 
   var ngauss_rdd = sc.parallelize(ngauss)
   var ngauss_rdd2 = ngauss_rdd.filter(x=>x > 4.0)
   // An operation without repartition
   var t0 = System.nanoTime()
   var c1 = ngauss_rdd2.count
   var t1 = System.nanoTime()
   var e1 = (t1 - t0)/1e9 // seconds
   // An operation with repartition
   var ngauss_rdd3 = ngauss_rdd2.repartition(1)
   t0 = System.nanoTime()
   var c2 = ngauss_rdd3.count
   t1 = System.nanoTime()
   var e2 = (t1 - t0)/1e9
   benchMat(i,::) := DenseVector[Double](Ns(i), e1, e2).t
}

/* Record the benchmark results */
csvwrite(new File("bench.csv"), benchMat)
