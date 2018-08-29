docker run \
  -it \
  --name ocaml \
  --mount type=bind,source=/home/vlad/Documents/TypeTheory,target=/ocaml \
  my_ocaml