## 2.2 Contexto con definiciones globales
Esquema de implementación:

- Análisis léxico de la definición
  - [x] Nombre = STRINGV
  - [x] Igualdad = EQ
  - [x] Valor = term    
- Análisis semántico de la definición.
  - **e.g.** x = 4
  - **e.g.** STRINGV EQ term
- Evaluar la definición
  - Guardar la definición y sobreescribirla si ya existe
    - Mirar si existe, borrar si existe, añadir nueva
  - Si la **_{ definición }_** usa otras definiciones:
    - Substituír las definiciones usadas en la nueva definición
    - Guardar la definición de la función sin hacer uso de _variables_