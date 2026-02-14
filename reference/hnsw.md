# VectrixDB HNSW Index

Hierarchical Navigable Small World graph for fast approximate nearest
neighbor search

Uses RcppAnnoy for high-performance ANN search. Falls back to
brute-force search if RcppAnnoy is not available.
