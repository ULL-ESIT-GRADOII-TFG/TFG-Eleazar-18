
Prioridades:

  - No establecer las direcciones de los argumentos en las funciones al generar el scope. Aplazarlo al runtime.
  - El tema de lo errores se debe mejorar.
    - Los errores internos del compilador, deben producir version+commit y localizacion de lanzamiento de excepcion.
    - Los errores del codigo compilado deben consolidarse. E imprimir correctamente la seccion y localizacion, Como un texto informativo del error

  - Implementar el tipo regex usar una librería pcre
    - Regex(regex) -- Constructor
    - matcher
    - ...


  - Implementar el config dentro del interprete
    - Pensar una forma de realizar interpolación de cadenas
      - Requiere un tipo nuevo LStr ([Object] -> String)
      - Se puede obviar por el momento construir un método interno para
        propósito específico

  - Realizar las comunicaciones con el API de Github
    - Mediante el propio lenguaje o de forma interna
