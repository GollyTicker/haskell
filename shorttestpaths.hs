


-- Djikstra, A*, BellmanFord etc..

{-
1  function Dijkstra(Graph, source):

       1. initialize each vertex with
                dist(v) = INF
                pred(v) = nil                                      // from source
 7      
 8      2. dist(src) := 0
 9      let Q = vertices Graph
10      
11      while Q is not empty:                                      // The main loop
12          let u = minimumBy (comparing dist) Q
            Q <- delete u Q
14          
            if dist(u) == INF then break while
17          
18          for each neighbor v of u:                              // where v has not yet been                                                 // removed from Q.
20              alt := dist[u] + dist_between(u, v) ;
21              if alt < dist[v]:                                  // Relax (u,v,a)
22                  dist[v]  := alt ;
23                  previous[v]  := u ;
24                  decrease-key v in Q;                           // Reorder v in the Queue (that is, heapify-down) 
25              end if
26          end for
27      end while
28      return dist[], previous[];
29  end function

-}