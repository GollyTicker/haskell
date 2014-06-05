@echo off 

ghc -O2 --make Main.hs -prof -auto-all -caf-all -fforce-recomp

echo "Running Tester...."

echo "Using smult..."
Main.exe %1 0 +RTS -p
echo Fin

echo "Using dmult..."
Main.exe %1 1 +RTS -p
echo "Fin"
