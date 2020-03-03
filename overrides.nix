{ pkgs }:
self: super: with pkgs.haskell.lib;
let callHackageDirect = args: self.callHackageDirect args {};

    ourOverrides = {
      http2 = callHackageDirect {
        pkg = "http2";
        ver = "2.0.3";
        sha256 = "14bqmxla0id956y37fpfx9v6crwxphbfxkl8v8annrs8ngfbhbr7";
      };

      warp = dontCheck (callHackageDirect {
        pkg = "warp";
        ver = "3.3.6";
        sha256 = "044w7ajkqlwnrpzc4zaqy284ac9wsklyby946jgfpqyjbj87985x";
      });

      warp-tls = callHackageDirect {
        pkg = "warp-tls";
        ver = "3.2.10";
        sha256 = "1zgr83zkb3q4qa03msfnncwxkmvk63gd8sqkbbd1cwhvjragn4mz";
      };

      servant = dontCheck (callHackageDirect {
        pkg = "servant";
        ver = "0.16.2";
        sha256 = "1a83fdcwnlkmyc6fqsrfd26izcgk1jgpdgyqma3l897cnnr62frs";
      });

      servant-client = dontCheck (callHackageDirect {
        pkg = "servant-client";
        ver = "0.16.0.1";
        sha256 = "1236sldcgvk2zj20cxib9yxrdxz7d1a83jfdnn9mxa272srfq9a9";
      });

      servant-client-core = dontCheck (callHackageDirect {
        pkg = "servant-client-core";
        ver = "0.16";
        sha256 = "0panxplcjslsvqxvsabn2fy0fhcqmmr0dqj51hk7bk7yyvgwxklf";
      });

      servant-server = dontCheck (callHackageDirect {
        pkg = "servant-server";
        ver = "0.16.2";
        sha256 = "1klcszpfqy1vn3q1wbqxjghfyddw8wbg4f0ggllqw8qx2f5zp5y1";
      });
    };
in
ourOverrides
