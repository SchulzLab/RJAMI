Sys.unsetenv("R_TESTS")

library(RJAMI)
library(Biobase)

load(system.file("extdata",
                 "test_data.RData",
                 package = "RJAMI"))

check_result <- function(result){
    result_triplets <- result$result
    result_aggregated <- result$aggregated

    expect_equal(nrow(result_triplets), 342)
    expect_equal(mean(result_triplets$CMI), 0.06986623,
                 tolerance = 1e-7)
    expect_equal(ncol(result_triplets), 6)

    expect_equal(nrow(result_aggregated), 90)
    expect_equal(mean(result_aggregated$p.adjusted.Fisher), 0.4495662,
                 tolerance = 1e-1)
    expect_equal(ncol(result_aggregated), 13)

}

context("TEST JAMI wrapper for various inputs")

test_that("JAMI works with file paths",{

    result <- jami(gene_miRNA_interactions =
                       system.file("extdata",
                                   "10_genes_mirna_interactions_triplet_format.txt",
                                   package = "RJAMI"),
                   gene_expr =
                       system.file("extdata",
                                   "10_genes_gene_expr.txt.gz",
                                   package = "RJAMI"),
                   mir_expr =
                       system.file("extdata",
                                   "10_genes_mir_expr.txt.gz",
                                   package = "RJAMI"),
                   output_file = tempfile())

    check_result(result)
})

test_that("JAMI works with expression data frames and triplets",{

    result <- jami(gene_miRNA_interactions = gene_mir_interactions_triplets,
                   gene_expr = gene_expr,
                   mir_expr = mir_expr,
                   output_file = tempfile())

    check_result(result)
})

test_that("JAMI works with expression data frames and set",{

    jami_settings(tripleFormat = FALSE)
    result <- jami(gene_miRNA_interactions = gene_mir_interactions_set,
                   gene_expr = gene_expr,
                   mir_expr = mir_expr,
                   output_file = tempfile())

    check_result(result)
})


test_that("JAMI works with expression matrices and triplets",{

    result <- jami(gene_miRNA_interactions = gene_mir_interactions_triplets,
                   gene_expr = as.matrix(gene_expr),
                   mir_expr = as.matrix(mir_expr),
                   output_file = tempfile())

    check_result(result)
})

test_that("JAMI works with Bioconductor ExpressionSet and triplets",{

    gene_expr_set <- Biobase::ExpressionSet(assayData = as.matrix(gene_expr))
    mir_expr_set <- Biobase::ExpressionSet(assayData = as.matrix(mir_expr))

    result <- jami(gene_miRNA_interactions = gene_mir_interactions_triplets,
                   gene_expr = gene_expr_set,
                   mir_expr = mir_expr_set,
                   output_file = tempfile())

    check_result(result)
})

