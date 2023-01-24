for f in `ls Day*.hs`; do
        echo "import qualified ${f%.*}"
done

echo main = do
for f in `ls Day*.hs`; do
        echo "    putStrLn \"$f\""
        echo "    ${f%.*}.main"
done
