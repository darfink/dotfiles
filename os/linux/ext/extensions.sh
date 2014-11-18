text=(
  application/javascript
  application/x-python
  application/x-sh
  text/asp
  text/css
  text/html
  text/plain
  text/x-asm
  text/x-c
  text/x-fortran
  text/x-h
  text/x-pascal
  text/x-python
  text/xml
)

media=(
  # Video formats
  video/3gp
  video/mp4
  video/mpeg
  video/ogg
  video/quicktime
  video/x-fli
  video/x-flv
  video/x-matroska
  video/x-ms-wmv
  video/x-msvideo

  # Audio formats
  audio/aiff
  audio/amr
  audio/flac
  audio/mpeg
  audio/ogg
  audio/vnd
  audio/wav
)

# Vim for text files
xdg-mime default vim.desktop "${text[@]}"

# VLC for media files
xdg-mime default vlc.desktop "${media[@]}"

# Xpdf for portable document format
xdg-mime default xpdf.desktop application/pdf