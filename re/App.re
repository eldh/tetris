open BsReactNative;

let app = () =>
  <View
    style=Style.(style([flex(1.), justifyContent(Center), alignItems(Center), backgroundColor(String("black"))]))>
    <SafeAreaView>
      <Field />
    </SafeAreaView>
  </View>;