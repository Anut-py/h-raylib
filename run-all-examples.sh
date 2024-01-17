set -e

if [[ $1 = "--help" ]] || [[ $1 = "-h" ]] || [[ $1 = "-?" ]]
then
  echo $'\nThis script compiles and runs all the examples in h-raylib.cabal\n'
  echo " -r    Only run the examples, do not compile"
  echo " -c    Run \`cabal clean\` before compiling"
  exit
fi

if [[ $1 != "-r" ]]
then
  if [[ $1 = "-c" ]]
  then
    cabal clean
  fi
  cabal build -j
fi

# core
cabal run basic-window
cabal run basic-automation-events
cabal run first-person-camera
cabal run camera-ray-collision

# textures
cabal run basic-images

# text
cabal run custom-font-text

# models
cabal run basic-models

# shaders
cabal run basic-shaders
cabal run postprocessing-effects

# audio
cabal run basic-audio

# rlgl
cabal run basic-rlgl
