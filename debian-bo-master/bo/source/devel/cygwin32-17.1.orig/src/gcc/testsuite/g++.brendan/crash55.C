      extern f(int);

      int& f(int x)
      {
          int local;

          local = x+2;
      
          return local;
      }
