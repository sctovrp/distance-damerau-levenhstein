# recibe las palabras y calcula su distancia
distanceDL <- function(a, b){
  
  word.a = strsplit(a,"")[[1]]
  word.b = strsplit(b,"")[[1]]

  table = matrix(0, nrow = nchar(a)+2, ncol = nchar(b)+2)
  table
  
  da = c(1:256)
  da[] = 0
  
  maxdist = nchar(a) + nchar(b)

  i <- 2
  while (i <= nchar(a) + 2) {
    table[i,1] <- maxdist
    table[i,2] <- i-2
    i = i + 1
  }
  table
  
  j <- 2
  while (j <= nchar(b) + 2) {
    table[1,j] <- maxdist
    table[2,j] <- j-2
    j = j + 1
  }
  table
  
  i <- 3
  while (i <= nchar(a) + 2){
    db = 0
    j <- 3 
    while (j <= nchar(b) + 2){
      k <- da[as.integer(charToRaw(word.b[j-2]))]
      l = db
      if (word.a[i-2] == word.b[j-2]){
        cost <- 0
        db = j-2
      } else {
        cost <- 1
      }
      table[i,j] = min(table[i-1,j-1] + cost, 
                       table[i,j-1] + 1, 
                       table[i-1,j] + 1,
                       table[k-1,l-1] + ((i-2)-k-1) + 1 + ((j-2) - l - 1))
      j = j + 1
    }
    da[as.integer(charToRaw(word.a[i-2]))] = i- 2
    i = i + 1
  }
  return(table[nchar(a)+2,nchar(b)+2])
}


  reserved.java = c("abstract",	"assert",	"boolean",	"break",	"byte",	"case",
                    "catch",	"char",	"class",	"const"	,"continue",	"default",
                    "double",	"do",	"else",	"enum",	"extends",	"false",
                    "final",	"finally",	"float",	"for",	"goto",	"if",
                    "implements",	"import",	"instanceof",	"int",	"interface",	"long",
                    "native",	"new",	"null",	"package",	"private",	"protected",
                    "public",	"return",	"short"	,"static",	"strictfp",	"super",
                    "switch",	"synchronized",	"this",	"throw",	"throws",	"transient",
                    "true", "try",	"void",	"volatile",	"while")
  reserved.c.chart = c("abstract",	"bool",	"continue",	"decimal"	,"default",
                       "event",	"explicit",	"extern"	,"char"	,"checked",
                       "class",	"const"	,"break",	"as",	"base",
                       "delegate",	"is",	"lock",	"long",	"num",
                       "byte"	,"case",	"catch"	,"false",	"finally",
                       "fixed",	"float"	,"for",	"foreach",
                       "goto"	,"if"	,"implicit",	"in",	"int",
                       "interface",	"internal",	"do",	"double",	"else",
                       "namespace",	"new",	"null"	,"object",	"operator",
                       "out",	"override"	,"params",	"private",	"protected",
                       "public",	"readonly",	"sealed",	"short"	,"sizeof",
                       "ref"	,"return"	,"sbyte",	"stackalloc"	,"static",
                       "string",	"struct",	"void"	,"volatile"	,"while",
                       "true"	,"try",	"switch",	"this",	"throw",
                       "unchecked"	,"unsafe"	,"ushort",	"using",	"using", "static",
                       "virtual",	"typeof",	"uint",	"ulong",	"out",
                       "add",	"alias",	"async"	,"await",	"dynamic",
                       "from"	,"get",	"orderby",	"ascending",	"descending",
                       "group",	"into",	"join",	"let"	,"nameof",
                       "global"	,"partial",	"set"	,"remove"	,"select",
                       "value",	"var"	,"when",	"Where",	"yield")
  reserved.c.plus = c("asm",	"else",	"new",	"this",
                      "auto",	"enum",	"operator"	,"throw",
                      "bool",	"explicit"	,"private",	"true",
                      "break",	"export",	"protected"	,"try",
                      "case",	"extern",	"public",	"typedef",
                      "catch",	"false",	"register",	"typeid",
                      "char",	"float"	,"reinterpret_cast",	"typename",
                      "class",	"for"	,"return"	,"union",
                      "const",	"friend",	"short"	,"unsigned",
                      "const_cast",	"goto",	"signed",	"using",
                      "continue"	,"if"	,"sizeof"	,"virtual",
                      "default",	"inline"	,"static"	,"void",
                      "delete",	"int",	"static_cast",	"volatile",
                      "do"	,"long",	"struct",	"wchar_t",
                      "double",	"mutable"	,"switch",	"while",
                      "dynamic_cast",	"namespace"	,"template",
                      "And",	"bitor",	"not_eq"	,"xor", "and_eq",	"compl",	"or",	"xor_eq",
                      "bitand",	"not",	"or_eq")
  print("Bienvenido al calculador de distancias:")
  
  # reserved words of java
  i <- 1
  result.java <- character()
  while (i < length(reserved.java)) {
    a = reserved.java[i]
    j <- i + 1
    while(j < length(reserved.java)){
      b = reserved.java[j]
      result.java  <- c(result.java, distanceDL(a,b))
      j = j + 1
    }
    i = i + 1
  }
  result.java = as.integer(result.java)
  
  # reserved words of c#
  i <- 1
  result.c.chart <- character()
  while (i < length(reserved.c.chart)) {
    a = reserved.c.chart[i]
    j <- i + 1
    while(j < length(reserved.c.chart)){
      b = reserved.c.chart[j]
      result.c.chart  <- c(result.c.chart, distanceDL(a,b))
      j = j + 1
    }
    i = i + 1
  }
  result.c.chart = as.integer(result.c.chart)
  
  # reserved words of c++
  i <- 1
  result.c.plus <- character()
  while (i < length(reserved.c.plus)) {
    a = reserved.c.plus[i]
    j <- i + 1
    while(j < length(reserved.c.plus)){
      b = reserved.c.plus[j]
      result.c.plus  <- c(result.c.plus, distanceDL(a,b))
      j = j + 1
    }
    i = i + 1
  }
  result.c.plus = as.integer(result.c.plus)


# Grafica de java
n1 <-floor(result.java)
t1 <- table(n1)
barplot(t1, main = "Grafica frecuencia de java", 
        xlab = "Distancias halladas", ylab = " Frecuencia de las distancias") 
# Grafica de c chart
n2 <-floor(result.c.chart)
t2 <- table(n2)
barplot(t2, main = "Grafica frecuencia de c chart", 
        xlab = "Distancias halladas", ylab = " Frecuencia de las distancias")
# Grafica de c++
n3 <-floor(result.c.plus)
t3 <- table(n3)
barplot(t3, main = "Grafica frecuencia de c++", 
        xlab = "Distancias halladas", ylab = " Frecuencia de las distancias")
par(mfrow=c(1,3))

# promedio y varianza de java
mean.java = mean(result.java)
var.java = var(result.java)
print("Promedio y varianza de java")
print(mean.java)
print(var.java)

# promedio y varianza de c chart
mean.c.chart = mean(result.c.chart)
var.c.chart = var(result.c.chart)
print("Promedio y varianza de c chart")
print(mean.c.chart)
print(var.c.chart)

# promedio y varianza de c++
mean.c.plus = mean(result.c.plus)
var.c.plus = var(result.c.plus)
print("Promedio y varianza de c++")
print(mean.c.plus)
print(var.c.plus)

