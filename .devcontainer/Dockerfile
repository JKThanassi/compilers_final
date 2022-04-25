FROM ocaml/opam:ubuntu-18.04-ocaml-4.13
# Avoid warnings by switching to noninteractive

USER root
# hadolint ignore=DL3008
RUN apt-get update \
  && apt-get install -y apt-transport-https && apt-get -y install llvm clang nasm valgrind gdb zsh


USER opam

run git clone https://github.com/longld/peda.git ~/peda
run echo "source ~/peda/peda.py" >> ~/.gdbinit 

# Install Opam packages
RUN opam update && \
  opam install \
  ounit batteries dune \
  ocamlformat ocaml-lsp-server extlib

