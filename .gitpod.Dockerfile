FROM gitpod/workspace-full

RUN bash -c  "sudo apt-get update && sudo apt-get install -y ghc ghc-prof ghc-doc"
