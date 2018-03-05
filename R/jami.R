#' Wrapper function for calling JAMI from R
#'
#' @param gene_miRNA_interactions either a path, a data frame, or a
#' list of interacions to consider
#' @param gene_expr either a path, a data frame, a matrix or
#' ExpressionSet for gene expression data
#' @param mir_expr either a path, a data frame, a matrix or
#' ExpressionSet for miRNA expression data
#' @param output_file optional output filename for JAMI results
#'
#' @return the JAMI result as a list of two data frames. result lists all
#' triplets, aggregated pools p-values by Fisher's method for each gene
#' @export
#'
#' @examples
jami <- function(gene_miRNA_interactions,
                 gene_expr, mir_expr, output_file = NULL){

    settingsManager <- jami_create_settings_manager()

    if(is.character(gene_expr)){
        if(!file.exists(gene_expr))
           stop("parameter gene_expr is not an expression matrix and if it is a path the file does not exist.")
        else gene_expr_file <- gene_expr
    } else{
        if(class(gene_expr) == "ExpressionSet"){
            gene_expr <- exprs(gene_expr)
        }
        message("writing gene expression matrix to temporary file")
        gene_expr_file <- tempfile(fileext = ".txt")
        write.table(gene_expr, file = gene_expr_file,
                    quote = FALSE,
                    sep = "\t",
                    row.names = TRUE,
                    col.names = TRUE)
    }

    if(is.character(mir_expr)){
        if(!file.exists(mir_expr))
            stop("parameter mir_expr is not an expression matrix and if it is a path the file does not exist.")
        else mir_expr_file <- mir_expr
    } else{
        if(class(mir_expr) == "ExpressionSet"){
            mir_expr <- exprs(mir_expr)
        }
        message("writing mir expression matrix to temporary file")
        mir_expr_file <- tempfile(fileext = ".txt")
        write.table(mir_expr, file = mir_expr_file,
                    quote = FALSE,
                    sep = "\t",
                    row.names = TRUE,
                    col.names = TRUE)
    }

    if(is.character(gene_miRNA_interactions)){
        if(!file.exists(gene_miRNA_interactions))
            stop("parameter gene_miRNA_interactions is not an expression matrix and if it is a path the file does not exist.")
        else gene_miRNA_interactions_file <- gene_miRNA_interactions
    } else{
        message("writing gene miRNA interactions to temporary file")
        gene_miRNA_interactions_file <- tempfile(fileext = ".txt")

        if(is.list(gene_miRNA_interactions) & !is.data.frame(gene_miRNA_interactions))
            gene_miRNA_interactions <- write_to_JAMI_set_format(
                gene_miRNA_interactions = gene_miRNA_interactions,
                output_file = gene_miRNA_interactions_file)
        else{
            write.table(gene_miRNA_interactions,
                        file = gene_miRNA_interactions_file,
                        quote = FALSE,
                        sep = "\t",
                        row.names = FALSE,
                        col.names = TRUE)
        }
    }

    if(is.null(output_file))
        output_file <- tempfile(fileext = ".txt")
    else{
        if(dir.exists(output_file)) stop("output file is a path")
    }

    completeRun <-
        .jnew("org.mpii.jami.CompleteRun",
          .jnew(class = "java.io.File",
                .jnew(class = "java.lang.String", gene_miRNA_interactions_file)),
          .jnew(class = "java.io.File",
                .jnew(class = "java.lang.String", gene_expr_file)),
          .jnew(class = "java.io.File",
                .jnew(class = "java.lang.String", mir_expr_file)),
          .jnew(class = "java.io.File",
                .jnew(class = "java.lang.String", output_file)),
          settingsManager)

    .jcall(completeRun, returnSig = "V",
        method = "runComputation")

    result <- read.delim(file = output_file, header = TRUE)
    result_aggregated <- read.delim(
        file = paste0(tools::file_path_sans_ext(output_file),
                      "_aggregated.txt"), header = TRUE)
    return(list(result = result, aggregated = result_aggregated))
}