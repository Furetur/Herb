FROM ocaml/opam

RUN sudo apt install -y lsb-release wget software-properties-common gnupg
RUN sudo bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
RUN alias clang=/usr/bin/clang-15

RUN opam install dune

# Development tools
RUN opam install ocaml-lsp-server ocamlformat