docker run \
  -it \
  --name ocaml \
  --mount type=bind,source=/Users/vladislav.zemtsov/Projects/TypeTheory,target=/ocaml \
  --name TypeTheory \
  my_ocaml