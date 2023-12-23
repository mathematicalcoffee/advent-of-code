#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int get_longest_path_length(
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
        weight_between_me_and_ni + get_longest_path_length(ni, finish_node, visited2, dist_matrix, n_vertices)
      );
    }
  }
  
  return weight;
}

// [[Rcpp::export]]
Rcpp::IntegerVector get_longest_path(
  int node,
  int finish_node,
  int step_index,
  const Rcpp::IntegerMatrix& dist_matrix,
  int n_vertices,
  Rcpp::IntegerVector& path
) {
  /* 
   * Returns a vector of length n_vertices + 1
   * First el is the distance of the longest path
   * the rest is the step on which that vertex is visited (couldn't think of something better)
  */
  if (node == finish_node) {
    return path;
  }
  
  Rcpp::IntegerVector longest_path (n_vertices + 1, 0);
  
  int ni;
  for (ni = 0; ni < n_vertices; ni++) {
    int weight_between_me_and_ni = dist_matrix(node, ni);
    if (weight_between_me_and_ni > 0 && path[ni + 1] == 0) {
      Rcpp::IntegerVector path2 = clone(path);
      path2[ni + 1] = step_index + 1;
      path2[0] = path2[0] + weight_between_me_and_ni;
      
      // weight is in element [0]
      Rcpp::IntegerVector candidate = get_longest_path(ni, finish_node, step_index + 1, dist_matrix, n_vertices, path2);
      if (candidate[0] > longest_path[0]) {
        longest_path = candidate;
      }
    }
  }
  return longest_path;
}
