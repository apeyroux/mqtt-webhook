self: super:
{
  haskellPackages = super.haskellPackages.override  {
    overrides = hself: hsuper:
      {   
        hashable = self.haskell.lib.dontHaddock hsuper.hashable;
      };  
  };  
}
