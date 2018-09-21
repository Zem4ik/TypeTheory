docker run \
  -it \
  --name ocaml \
  --mount type=bind,source=/home/vlad/Projects,target=/ocaml \
  --name TypeTheory \
  my_ocaml