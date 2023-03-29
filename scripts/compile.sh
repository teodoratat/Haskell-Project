if [ ! -d ../out ] ; then
    mkdir ../out
fi
cd ../src
ghc -O2 Main.hs -outputdir ../out -o ../../out/raytracer
ghc -O2 Test/Tests.hs -main-is Test.Tests -outputdir ../../out -o ../../out/tests
cd ../scripts