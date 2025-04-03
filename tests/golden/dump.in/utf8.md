Some code:

    hello-exe-hello> src/hello.hs:23:35: error:
    hello-exe-hello>     • Couldn't match type ‘Password’ with ‘BuiltinData’
    hello-exe-hello>       Expected type: template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.Q
    hello-exe-hello>                        (template-haskell-2.16.0.0:Language.Haskell.TH.Syntax.TExp
    hello-exe-hello>                           (PlutusTx.Code.CompiledCode
    hello-exe-hello>                              (BuiltinData -> BuiltinData -> BuiltinData -> ())))
    hello-exe-hello>         Actual type: th-compat-0.1.4:Language.Haskell.TH.Syntax.Compat.SpliceQ
    hello-exe-hello>                        (PlutusTx.Code.CompiledCode
    hello-exe-hello>                           (Password -> Password -> BuiltinData -> ()))
    hello-exe-hello>     • In the expression: compile [|| validator ||]
    hello-exe-hello>       In the Template Haskell splice $$(compile [|| validator ||])
    hello-exe-hello>       In the first argument of ‘mkValidatorScript’, namely
    hello-exe-hello>         ‘$$(compile [|| validator ||])’
