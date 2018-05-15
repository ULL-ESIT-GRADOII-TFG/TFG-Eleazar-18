
Prioridades:

  - La implementacion de las clases. Se debe revisar ya que el intento de
    reutilizacion del AST no es lo más adecuado
    - [Method (Ref, Prog) | Var Ref ]

    - Seek por un __init__ :: args -> (), generar un ClassName :: args -> Object

    - Definir los métodos especiales por los que buscar dentro de una clase
      - __print__
      - __map__
      - __bool__
      - ...

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
