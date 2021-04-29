Todos os arquivos têm formato SSV (“space-separated values”).
* Valores são separados por espaço.
* Valores com aspas (`"`) devem tê-las duplicadas.
* Valores com espaços devem ser delimitados por aspas.

Exemplo com os dois últimos casos acima: `ele disse "olá"` -> `"ele disse ""olá"""`
* Valores vazios devem ser representados por um par de aspas.
* Linhas começando por `#` são ignoradas.
* Valores que são o primeiro da linha e começam por `#` devem ser delimitados por aspas.

Todos os valores numéricos não inteiros usam vírgula como separador decimal.

## Gramáticas
Elementos `LINHA` ou começando com `LINHA-` representam uma linha inteira no arquivo.
Elementos entre um par de colchetes representam um valor.
* `X?` - `X` ocorre zero ou uma vez
* `X*` - `X` ocorre zero ou mais vezes
* `X:s` - `X` é uma ‘string’
* `X:i` - `X` é um número inteiro sem sinal
* `X:n` - `X` é um número sem sinal
* `X:sn` - `X` é um número possivelmente com sinal

# Padronização de Nomes
**Caminho:** data/names.ssv

Nomes encontrados em notas de corretagem são substituídos pelo nome padrão. 

```
CONTEÚDO := LINHA*
LINHA := [NOME-PADRÃO:s] [OUTRO-NOME:s]*
```

# Tipos
**Caminho:** data/types.ssv

Classifica o tipo dos ativos. Ativos não listados no arquivo são considerados ações comuns ou units.

```
CONTEÚDO := LINHA*
LINHA := [NOME:s] [TIPO]
TIPO := 'ETF' /* ETF de renda variável */
      | 'ETFRF' /* ETF de renda fixa */
      | 'FII' /* fundo imobiliário */
```

# Nota de Corretagem
**Caminho:** data/`ANO`/`DATA` - `CORRETORA`.ssv

```
CONTEÚDO :=
    LINHA-DE-NEGOCIAÇÃO*
    LINHA-EM-BRANCO
    LINHA-DE-CUSTO*
    LINHA-EM-BRANCO
    LINHA-DE-VALOR-DA-NOTA
LINHA-DE-NEGOCIAÇÃO := [OPERAÇÃO]  [NOME:s]  [QUANTIA:i]  [PREÇO:n]
OPERAÇÃO := 'C' /* compra */ | 'V' /* venda */
LINHA-DE-CUSTO := [DESCRIÇÃO:s] /* ignorada */  [CUSTO:n]  /* não incluir IRRF! */
LINHA-DE-VALOR-DA-NOTA := [VALOR:sn] /* positivo se houve mais vendas, negativo se houve mais compras */
```

# Eventos
**Caminho:** data/`ANO`/`DATA` - EVENTS.ssv

```
CONTEÚDO := LINHA*
LINHA := DE  ['->']  PARA
DE := [QUANTIA:i]  [NOME:s]
PARA := [QUANTIA:i]  [NOME:s]  [PREÇO-MÉDIO] (['+'] [QUANTIA:i]  [NOME:s]  [PREÇO-MÉDIO])*
PREÇO-MÉDIO := PREÇO:n
             | PORCENTAGEM:n '%'
             | MULTIPLICADOR:n 'x'
             | NUMERADOR:i '/' DENOMINADOR:i
```

## Exemplos
### Desdobramento (_Split_)
Desdobramento de 1 para 4:
```
1  MGLU3  ->  4  MGLU3  1/4
```
Cada `1` ação `MGLU3` com preço médio `pm` é substituída por `4` ações `MGLU3` com preço médio de `1/4` de `pm`.

### Grupamento (_Inplit_)
Grupamento de 4 para 1:
```
4  ABCD3  ->  1  ABCD3  4x
```
Cada `4` ações `ABCD3` com preço médio `pm` é substituída por `1` ação `ABCD3` com preço médio de `4` vezes `pm`.

### Bonificação
Bonificação de 1 ação de valor R$ 12,34 para cada 10 ações:
```
10  ABCD3  ->  10  ABCD3  1x  +  1  ABCD  12,34
```
Cada `10` ações `ABCD3` com preço médio `pm` é substituída por `10` ações `ABCD` com o mesmo preço médio `pm`
**mais** `1` ação `ABCD3` com preço médio de `R$ 12,34`.

### Cisão
```
1  PCAR3  ->  1  PCAR3  82,3%  +  1  ASAI3  17,7%
```
Cada `1` ação `PCAR3` com preço médio `pm` é substituída por `1` ação `PCAR3` com preço médio de `82,3%` de `pm`
**mais** `1` ação `ASAI3` com preço médio de `17,7%` de `pm`.

# Bens
**Gerado automaticamente!**

**Caminho:** data/`ANO`/END - assets.ssv

Representa os bens no fim do ano.
```
CONTEÚDO := LINHA*
LINHA := [NOME:s]  [QUANTIA:i]  [PREÇO-MÉDIO:n]  [CORRETORA:s]
```