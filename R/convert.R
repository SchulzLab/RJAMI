#' convert triplet to set format
#'
#' @param gene_miRNA_interactions a data frame with one gene-miRNA interaction per row
#'
#' @return a list of genes where each entry is a vector of miRNAs
#' @export
#'
#' @examples convert_triplet_to_set_format(gene_mir_interactions_triplet)
convert_triplet_to_set_format <- function(gene_miRNA_interactions){
    genes <- unique(c(
        as.character(gene_miRNA_interactions[,1]),
        as.character(gene_miRNA_interactions[,2])))

    list_of_interactions <-
    lapply(genes, function(gene) union(
            gene_miRNA_interactions[which(gene_miRNA_interactions[,1] == gene),3],
            gene_miRNA_interactions[which(gene_miRNA_interactions[,2] == gene),3]
    ))
    names(list_of_interactions) <- genes

    return(list_of_interactions)
}

#' write list of gene miRNA interactions to JAMI set format
#'
#' @param gene_miRNA_interactions a list of genes where each entry is a vector of miRNAs
#' @param output_file where the file should be written to
#'
#' @return nothing
#' @export
#'
#' @examples write_to_JAMI_set_format(gene_mir_interactions_set, tempfile())
write_to_JAMI_set_format <- function(gene_miRNA_interactions, output_file){

    gene_miRNA_interactions_collapsed <- cbind(
        names(gene_miRNA_interactions),
        do.call(rbind, lapply(gene_miRNA_interactions, function(gene_mirs){
        paste(gene_mirs, collapse = ",")
    })))

    write.table(gene_miRNA_interactions_collapsed,
                file = output_file,
                quote = FALSE,
                sep = "\t",
                col.names = c("gene", "miRNAs"),
                row.names = FALSE)
}