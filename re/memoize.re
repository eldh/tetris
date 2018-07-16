let cache = (f: 'a => 'b) => {
  let value = ref();
  let arg = ref();
  (a: 'a) =>
    switch a {
    | a when a === arg =>
      value^;
    | _ =>
      value := f(a);
      arg := a^;
      value^;
    };
};
