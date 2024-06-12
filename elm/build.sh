bash ./optimized.sh src/Main.elm build/elm.min.js
mv build/elm.min.js ../public/js/
rm -rf build
