ghc -O2 --make Main.hs -prof -auto-all -caf-all -fforce-recomp
Main.exe %1 +RTS -p
