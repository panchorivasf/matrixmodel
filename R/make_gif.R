#' Create GIF Animation from Images in Folder
#'
#' Reads all images of specified format from a folder and creates an animated GIF.
#'
#' @param in_folder Character. Path to folder containing input images. Default is NULL.
#' @param out_folder Character. Path to folder for output GIF. Default is NULL.
#' @param out_name Character. A name for the output GIF file, without extension.
#' @param format Character. Image format to look for (e.g., "png", "jpg"). Default is "png".
#' @param fps Numeric. Frames per second for the animation. Default is 2.
#'
#' @return Creates a GIF file in the specified output folder. No return value.
#'
#' @examples
#' \dontrun{
#' make_gif(in_folder = "path/to/images",
#'          out_folder = "path/to/output",
#'          format = "png",
#'          fps = 5)
#' }
#'
#' @importFrom magick image_read image_join image_animate image_write
#' @export
make_gif <- function(in_folder = NULL,
                     out_folder = NULL,
                     out_name = "animation",
                     format = "png",
                     fps = 2
) {
  # Read all images from folder
  img_files <- list.files(path = in_folder,
                          pattern = paste0("*.",format),
                          full.names = TRUE)

  # Sort files alphabetically
  img_files <- sort(img_files)

  cat("Reading images...\n")
  img_list <- lapply(img_files, magick::image_read)
  # Join images
  img_joined <- magick::image_join(img_list)
  # Animate
  img_animated <- magick::image_animate(img_joined,
                                        fps = fps)
  # Write GIF
  magick::image_write(img_animated, paste0(out_folder,"/",
                                           out_name,".gif"))
  cat("GIF written in", out_folder)
}
