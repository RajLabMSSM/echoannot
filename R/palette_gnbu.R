palette_gnbu <- function(n=NULL){ 
    # custom_colors <- RColorBrewer::brewer.pal(
    #     n = length(levels(bin_counts$bin)), "GnBu"
    # )
    colors <- c("#F7FCF0","#E0F3DB","#CCEBC5","#A8DDB5",
                "#7BCCC4","#4EB3D3","#2B8CBE","#0868AC","#084081")
    if(!is.null(n)){
        colors <- colors[seq_len(min(length(colors),n))]
    } 
    return(colors)
}