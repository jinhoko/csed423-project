class MyClass {
  do(arg:Object) : Object {
      {     let o3:SELF_TYPE <- 5 in 1; 
            let o3:SELF_TYPE <- new Int in 1;
            let o3:Object <- new SELF_TYPE in 1;
            let o3:SELF_TYPE <- new Object in 1;
            let o3:Object <- new Object in 1; 
            let o3:SELF_TYPE <- new SELF_TYPE in 1;
            let o3:MyClass <- new MyClass in 1;
            let o3:MyClassChild <- new MyClass in 1;
            let o3:MyClass <- new MyClassChild in 1;
            let o3:YourClass <- new Myclass in 1;
            let o3:MyClass <- new YourClass in 1;
      }
  };
};

class MyClassChild inherits MyClass  {
      do(arg:Object) : Object { 1};
};

class YourClass  {
      do(arg:Object) : Object { 1};
};