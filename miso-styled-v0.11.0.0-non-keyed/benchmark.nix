{ mkDerivation, base, containers, miso, miso-style, stdenv, vector
}:
mkDerivation {
  pname = "benchmark";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers miso miso-style vector
  ];
  description = "JS Framework Benchmark for Miso";
  license = stdenv.lib.licenses.bsd3;
}
