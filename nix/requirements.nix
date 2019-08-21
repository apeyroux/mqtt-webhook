# generated using pypi2nix tool (version: 2.0.0)
# See more at: https://github.com/garbas/pypi2nix
#
# COMMAND:
#   pypi2nix -r ../requirements.txt -V3.7 -e setuptools_scm
#

{ pkgs ? import <nixpkgs> {},
  overrides ? ({ pkgs, python }: self: super: {})
}:

let

  inherit (pkgs) makeWrapper;
  inherit (pkgs.stdenv.lib) fix' extends inNixShell;

  pythonPackages =
  import "${toString pkgs.path}/pkgs/top-level/python-packages.nix" {
    inherit pkgs;
    inherit (pkgs) stdenv;
    python = pkgs.python37;
    # patching pip so it does not try to remove files when running nix-shell
    overrides =
      self: super: {
        bootstrapped-pip = super.bootstrapped-pip.overrideDerivation (old: {
          patchPhase = old.patchPhase + ''
            if [ -e $out/${pkgs.python37.sitePackages}/pip/req/req_install.py ]; then
              sed -i \
                -e "s|paths_to_remove.remove(auto_confirm)|#paths_to_remove.remove(auto_confirm)|"  \
                -e "s|self.uninstalled = paths_to_remove|#self.uninstalled = paths_to_remove|"  \
                $out/${pkgs.python37.sitePackages}/pip/req/req_install.py
            fi
          '';
        });
      };
  };

  commonBuildInputs = [];
  commonDoCheck = false;

  withPackages = pkgs':
    let
      pkgs = builtins.removeAttrs pkgs' ["__unfix__"];
      interpreterWithPackages = selectPkgsFn: pythonPackages.buildPythonPackage {
        name = "python37-interpreter";
        buildInputs = [ makeWrapper ] ++ (selectPkgsFn pkgs);
        buildCommand = ''
          mkdir -p $out/bin
          ln -s ${pythonPackages.python.interpreter} \
              $out/bin/${pythonPackages.python.executable}
          for dep in ${builtins.concatStringsSep " "
              (selectPkgsFn pkgs)}; do
            if [ -d "$dep/bin" ]; then
              for prog in "$dep/bin/"*; do
                if [ -x "$prog" ] && [ -f "$prog" ]; then
                  ln -s $prog $out/bin/`basename $prog`
                fi
              done
            fi
          done
          for prog in "$out/bin/"*; do
            wrapProgram "$prog" --prefix PYTHONPATH : "$PYTHONPATH"
          done
          pushd $out/bin
          ln -s ${pythonPackages.python.executable} python
          ln -s ${pythonPackages.python.executable} \
              python3
          popd
        '';
        passthru.interpreter = pythonPackages.python;
      };

      interpreter = interpreterWithPackages builtins.attrValues;
    in {
      __old = pythonPackages;
      inherit interpreter;
      inherit interpreterWithPackages;
      mkDerivation = args: pythonPackages.buildPythonPackage (args // {
        nativeBuildInputs = (args.nativeBuildInputs or []) ++ args.buildInputs;
      });
      packages = pkgs;
      overrideDerivation = drv: f:
        pythonPackages.buildPythonPackage (
          drv.drvAttrs // f drv.drvAttrs // { meta = drv.meta; }
        );
      withPackages = pkgs'':
        withPackages (pkgs // pkgs'');
    };

  python = withPackages {};

  generated = self: {
    "APScheduler" = python.mkDerivation {
      name = "APScheduler-3.6.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/21/59/20a05dfa5525df11144f68a972b9b25aeeca4933dcbf23510c07955117e4/APScheduler-3.6.1.tar.gz";
        sha256 = "529afb7909e08416132891188cbfea5351eb35e4a684b67e983d819e8d01a6b0";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [
        self."setuptools-scm"
      ];
      propagatedBuildInputs = [
        self."SQLAlchemy"
        self."pytz"
        self."six"
        self."tzlocal"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/agronholm/apscheduler";
        license = "MIT";
        description = "In-process task scheduler with Cron-like capabilities";
      };
    };

    "Click" = python.mkDerivation {
      name = "Click-7.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/f8/5c/f60e9d8a1e77005f664b76ff8aeaee5bc05d0a91798afd7f53fc998dbc47/Click-7.0.tar.gz";
        sha256 = "5b94b49521f6456670fdb30cd82a4eca9412788a93fa6dd6df72c94d5a8ff2d7";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://palletsprojects.com/p/click/";
        license = licenses.bsdOriginal;
        description = "Composable command line interface toolkit";
      };
    };

    "Flask" = python.mkDerivation {
      name = "Flask-1.1.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/2e/80/3726a729de758513fd3dbc64e93098eb009c49305a97c6751de55b20b694/Flask-1.1.1.tar.gz";
        sha256 = "13f9f196f330c7c2c5d7a5cf91af894110ca0215ac051b5844701f2bfd934d52";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."Click"
        self."Jinja2"
        self."Werkzeug"
        self."itsdangerous"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://palletsprojects.com/p/flask/";
        license = "BSD-3-Clause";
        description = "A simple framework for building complex web applications.";
      };
    };

    "Flask-MonitoringDashboard" = python.mkDerivation {
      name = "Flask-MonitoringDashboard-2.1.4";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/30/b8/91d8be7c32b04955bfdaeb6b6ee4d1fbc26d3a2959016d46460940229baf/Flask-MonitoringDashboard-2.1.4.tar.gz";
        sha256 = "e73c771a5fc922cd1d577b39c4335d3f6d32ce3530df1d2a870b3bc1a927b08a";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."APScheduler"
        self."Click"
        self."Flask"
        self."SQLAlchemy"
        self."colorhash"
        self."configparser"
        self."numpy"
        self."psutil"
        self."pytz"
        self."tzlocal"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/flask-dashboard/Flask-MonitoringDashboard";
        license = "UNKNOWN";
        description = "Automatically monitor the evolving performance of Flask/Python web services.";
      };
    };

    "Jinja2" = python.mkDerivation {
      name = "Jinja2-2.10.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/93/ea/d884a06f8c7f9b7afbc8138b762e80479fb17aedbbe2b06515a12de9378d/Jinja2-2.10.1.tar.gz";
        sha256 = "065c4f02ebe7f7cf559e49ee5a95fb800a9e4528727aec6f24402a5374c65013";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."MarkupSafe"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://jinja.pocoo.org/";
        license = licenses.bsdOriginal;
        description = "A small but fast and easy to use stand-alone template engine written in pure python.";
      };
    };

    "MarkupSafe" = python.mkDerivation {
      name = "MarkupSafe-1.1.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/b9/2e/64db92e53b86efccfaea71321f597fa2e1b2bd3853d8ce658568f7a13094/MarkupSafe-1.1.1.tar.gz";
        sha256 = "29872e92839765e546828bb7754a68c418d927cd064fd4708fab9fe9c8bb116b";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://palletsprojects.com/p/markupsafe/";
        license = "BSD-3-Clause";
        description = "Safely add untrusted strings to HTML/XML markup.";
      };
    };

    "SQLAlchemy" = python.mkDerivation {
      name = "SQLAlchemy-1.3.7";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/c2/75/6217c626fa22ad56ae5ccb1a36e7c4f17f5ca31543887e00179468d10464/SQLAlchemy-1.3.7.tar.gz";
        sha256 = "0459bf0ea6478f3e904de074d65769a11d74cdc34438ab3159250c96d089aef0";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://www.sqlalchemy.org";
        license = "MIT";
        description = "Database Abstraction Library";
      };
    };

    "Werkzeug" = python.mkDerivation {
      name = "Werkzeug-0.15.5";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/c7/fb/56734c47bc5bb4d00898c2581bc08166cb6fea72b6894cf279053521c25a/Werkzeug-0.15.5.tar.gz";
        sha256 = "a13b74dd3c45f758d4ebdb224be8f1ab8ef58b3c0ffc1783a8c7d9f4f50227e6";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://palletsprojects.com/p/werkzeug/";
        license = "BSD-3-Clause";
        description = "The comprehensive WSGI web application library.";
      };
    };

    "colorhash" = python.mkDerivation {
      name = "colorhash-1.0.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/bf/e9/b1d948c518ac6f683711a38a9c5bb84597f417ac2fc9296a49c40c1b2dab/colorhash-1.0.2.tar.bz2";
        sha256 = "e0f925c3d82d5ed6e5fe8039492325263b5b75490bddf38e24a221467c3ee764";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://bitbucket.org/fk/python-color-hash";
        license = "MIT";
        description = "Generate a color based on a value";
      };
    };

    "configparser" = python.mkDerivation {
      name = "configparser-3.8.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/b1/83/fa54eee6643ffb30ab5a5bebdb523c697363658e46b85729e3d587a3765e/configparser-3.8.1.tar.gz";
        sha256 = "bc37850f0cc42a1725a796ef7d92690651bf1af37d744cc63161dac62cabee17";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/jaraco/configparser/";
        license = "UNKNOWN";
        description = "Updated configparser from Python 3.7 for Python 2.6+.";
      };
    };

    "itsdangerous" = python.mkDerivation {
      name = "itsdangerous-1.1.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/68/1a/f27de07a8a304ad5fa817bbe383d1238ac4396da447fa11ed937039fa04b/itsdangerous-1.1.0.tar.gz";
        sha256 = "321b033d07f2a4136d3ec762eac9f16a10ccd60f53c0c91af90217ace7ba1f19";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://palletsprojects.com/p/itsdangerous/";
        license = licenses.bsdOriginal;
        description = "Various helpers to pass data to untrusted environments and back.";
      };
    };

    "numpy" = python.mkDerivation {
      name = "numpy-1.17.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/da/32/1b8f2bb5fb50e4db68543eb85ce37b9fa6660cd05b58bddfafafa7ed62da/numpy-1.17.0.zip";
        sha256 = "951fefe2fb73f84c620bec4e001e80a80ddaa1b84dce244ded7f1e0cbe0ed34a";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://www.numpy.org";
        license = licenses.bsdOriginal;
        description = "NumPy is the fundamental package for array computing with Python.";
      };
    };

    "pampy" = python.mkDerivation {
      name = "pampy-0.2.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/35/00/e2d1f884a5ccad04c03fed6ba538bc41ba9883c5d6dbe16d52a730d3fe48/pampy-0.2.1.tar.gz";
        sha256 = "42922abf2902f06cf4cff65409aa03e773fd15e3540fb24719207e3927a3dced";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/santinic/pampy";
        license = "UNKNOWN";
        description = "The Pattern Matching for Python you always dreamed of";
      };
    };

    "psutil" = python.mkDerivation {
      name = "psutil-5.6.3";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/1c/ca/5b8c1fe032a458c2c4bcbe509d1401dca9dda35c7fc46b36bb81c2834740/psutil-5.6.3.tar.gz";
        sha256 = "863a85c1c0a5103a12c05a35e59d336e1d665747e531256e061213e2e90f63f3";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/giampaolo/psutil";
        license = licenses.bsdOriginal;
        description = "Cross-platform lib for process and system monitoring in Python.";
      };
    };

    "pytz" = python.mkDerivation {
      name = "pytz-2019.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/27/c0/fbd352ca76050952a03db776d241959d5a2ee1abddfeb9e2a53fdb489be4/pytz-2019.2.tar.gz";
        sha256 = "26c0b32e437e54a18161324a2fca3c4b9846b74a8dccddd843113109e1116b32";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://pythonhosted.org/pytz";
        license = "MIT";
        description = "World timezone definitions, modern and historical";
      };
    };

    "setuptools-scm" = python.mkDerivation {
      name = "setuptools-scm-3.3.3";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/83/44/53cad68ce686585d12222e6769682c4bdb9686808d2739671f9175e2938b/setuptools_scm-3.3.3.tar.gz";
        sha256 = "bd25e1fb5e4d603dcf490f1fde40fb4c595b357795674c3e5cb7f6217ab39ea5";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pypa/setuptools_scm/";
        license = "MIT";
        description = "the blessed package to manage your versions by scm tags";
      };
    };

    "six" = python.mkDerivation {
      name = "six-1.12.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/dd/bf/4138e7bfb757de47d1f4b6994648ec67a51efe58fa907c1e11e350cddfca/six-1.12.0.tar.gz";
        sha256 = "d16a0141ec1a18405cd4ce8b4613101da75da0e9a7aec5bdd4fa804d0e0eba73";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/benjaminp/six";
        license = "MIT";
        description = "Python 2 and 3 compatibility utilities";
      };
    };

    "tzlocal" = python.mkDerivation {
      name = "tzlocal-2.0.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/c6/52/5ec375d4efcbe4e31805f3c4b301bdfcff9dcbdb3605d4b79e117a16b38d/tzlocal-2.0.0.tar.gz";
        sha256 = "949b9dd5ba4be17190a80c0268167d7e6c92c62b30026cf9764caf3e308e5590";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."pytz"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/regebro/tzlocal";
        license = "MIT";
        description = "tzinfo object for the local timezone";
      };
    };
  };
  localOverridesFile = ./requirements_override.nix;
  localOverrides = import localOverridesFile { inherit pkgs python; };
  commonOverrides = [
    
  ];
  paramOverrides = [
    (overrides { inherit pkgs python; })
  ];
  allOverrides =
    (if (builtins.pathExists localOverridesFile)
     then [localOverrides] else [] ) ++ commonOverrides ++ paramOverrides;

in python.withPackages
   (fix' (pkgs.lib.fold
            extends
            generated
            allOverrides
         )
   )