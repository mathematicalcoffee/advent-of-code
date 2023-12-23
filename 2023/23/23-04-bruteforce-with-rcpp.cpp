#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int get_longest_path(
  int node,
  int finish_node,
  Rcpp::LogicalVector& visited,
  const Rcpp::IntegerMatrix& dist_matrix,
  int n_vertices
) {
  if (node == finish_node)
    return 0;
  
  int weight = 0;
  int ni;
  for (ni = 0; ni < n_vertices; ni++) {
    int weight_between_me_and_ni = dist_matrix(node, ni);
    if (weight_between_me_and_ni > 0 && !visited[ni]) {
      // std::cout << "visiting " << ni << " from " << node << " " << weight_between_me_and_ni << "\n";
      // copy visited
      Rcpp::LogicalVector visited2 = clone(visited);
      visited2[ni] = true;
      weight = std::max(
        weight,
        weight_between_me_and_ni + get_longest_path(ni, finish_node, visited2, dist_matrix, n_vertices)
      );
    }
  }
  
  return weight;
}
