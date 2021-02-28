# pre-requirements
nodejs (10+) and npm should be available in system

Installing dependencies for build process
```cd js && npm i```

# commands

All commands should be invoked in `/js` subfolder.

`npm run dev` - starts rollup in dev mode. After any change js files will be recompiled and updated in `priv/` folder
In that mode after recompile updated assets will be served with cowboy.
In dev mode source maps provided.

`npm run build` - compiles minimized production bundle for JS. Currently, sourcemaps included in production build.

# libs
`src/libs` - all js files in that folder will be proceeded and bundled into the same name js file that mixes module 
export to window object.

The goals of such behaviour - rewrite static libs to less minimized way (use meaningful names for variables etc).

Because of using modules in IIFE resulting code will be run in 
[strict mode](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode), that will improve JS 
typing and also will help in debugging of new features.

Also, all functions\variables that should be bound to window object should be marked with "export". In common way using
Window object s namespace isn't good idea, and using modules is a step to using imports for dep resolution instead of
depending on load order in html. Also, explicit requirement of export will help to prevent global namespace pollution.


# status
As an example bert.js processed by rollup.
PR includes original and processed bert.js in priv folder.
During conversion of bert.js to module 3 global variables found. In module implementation they are scoped to module.

# todo
this PR only proposal, in real one we should remove compiled js from repo adding compiled artifacts to gitignore
also build pipeline should be updated to reflect additional compilation step.
