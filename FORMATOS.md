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
CONTEÚDO := NOTA-DE-CORRETAGEM  MAIS-NOTAS-DE-CORRETAGEM*
NOTA-DE-CORRETAGEM :=
    LINHA-DE-NEGOCIAÇÃO*
    LINHA-EM-BRANCO
    LINHA-DE-CUSTO*
    LINHA-EM-BRANCO
    LINHA-DE-VALOR-DA-NOTA
LINHA-DE-NEGOCIAÇÃO := [OPERAÇÃO-COMUM]  [NOME:s]  [QUANTIA:i]  [PREÇO:n]
                     | [OPERAÇÃO-EXERCÍCIO]  [OPÇÃO:s]  [NOME:s]  [QUANTIA:i]  [PREÇO:n]
OPERAÇÃO-COMUM := 'C' /* compra */ | 'V' /* venda */
OPERAÇÃO-EXERCÍCIO := 'EC' /* compra */ | 'EV' /* venda */
LINHA-DE-CUSTO := [DESCRIÇÃO:s] /* ignorada */  [CUSTO:n]  /* não incluir IRRF! */
LINHA-DE-VALOR-DA-NOTA := [VALOR:sn] /* positivo se houve mais vendas, negativo se houve mais compras */
MAIS-NOTAS-DE-CORRETAGEM :=
    LINHA-EM-BRANCO
    NOTA-DE-CORRETAGEM
```

# Eventos
**Caminho:** data/`ANO`/`DATA` - EVENTS.ssv

```
CONTEÚDO := LINHA*
LINHA := 'convert'  [NOME:s]  [QUANTIA:i]  ->  [QUANTIA:i]  [NOME:s]
       | 'bonus'  [NOME:s]  [QUANTIA:i]  ->  [QUANTIA:i]  [NOME:s]  [PREÇO:n]
```

## Exemplos
### Desdobramento (_Split_)
Desdobramento de 1 para 4:
```
convert  MGLU3  1  ->  4  MGLU3
```
Cada `1` ação `MGLU3` com preço médio `pm` é substituída por `4` ações `MGLU3` com preço médio de `1/4` de `pm`.

### Grupamento (_Inplit_)
Grupamento de 4 para 1:
```
convert  ABCD3  4  ->  1  ABCD3
```
Cada `4` ações `ABCD3` com preço médio `pm` é substituída por `1` ação `ABCD3` com preço médio de `4` vezes `pm`.

### Bonificação
Bonificação de 1 ação de valor R$ 12,34 para cada 10 ações:
```
bonus  ABCD3  10  ->  1  ABCD3  12,34
```
Para cada `10` ações `ABCD3`, `1` ação `ABCD3` com preço `R$ 12,34` é incluída.

### Cisão
```
bonus  PCAR3  1  ->  1  ASAI3  14,70
```
Para cada `1` ação `PCAR3`, `1` ação `ASAI3` com preço `R$ 14,70` é incluída.
