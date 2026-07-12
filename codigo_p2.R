#Seja X uma variável aleatória de uma população com função de densidade

#3/32 * x ^ 5, se 0 <= x <= 2
#0 c.c

#a. Gere valores pseudo-aleatórios de X1, X2,..., Xn, uma amostra aleatória da população acima definida, e calcule a proporcão de valores entre 0.75 e 1.95. Compare os valores obtidos empiricamente para as amostras sugeridas junto com o valor teórico calculado;

#b. Use os valores gerados no item anterior para aproximar empiricamente a distribuição da mediana para calcular um estimador intervalar de 95% de confiança para a mediana amostral.

#Obs: Use o método da transformação inversa para gerar valores pseudo-aleatórios da amostra aleatória X1... Xn
#----#
#FDA = 1/64 * x ^ 6, 0 <= x <= 2
#FDA inversa = (64 * x) ^ (1/6) 0 <= x <= 1
#P(0.75 < x < 1.95) = 0,85628738671875
#P(X <= a) = 0.5 -> a = 1,781797436280678609
#----------------------#
#constantes e sementes
set.seed(524922)
mediana_teorica <- (64 * 0.5) ^ (1/6)
proporcao_teorica <- ((1/64) * (1.95 ^ 6)) - ((1/64) * (0.75 ^ 6))

#funcoes
# função geradora de dados
gerar_amostra <- function(n) {
	x <- runif(n, min = 0, max = 1)
dados <- (64 * x) ^ (1/6)
dados
}

# função n repetições do experimento
many_rep <- function(rep,n) {
	#iniciando vetores e outras coisas
proporcoes_amostrais <- vector(length = rep)
medianas_amostrais <- vector(length = rep)
mediana_teorica_no_ic <- "Não"
prop_teorica_no_ic <- "Não"

i <- 0
# loop para uhhhh criar os vetores das proporções amostrais, e medianas amostrais.
repeat {
	if (i > rep) {break }
#---#
amostra <- gerar_amostra(n)
# mediana!
medianas_amostrais[i] <- median(amostra)
# proporção...
esta_no_intervalo <- (0.75 < amostra) & (amostra < 1.95)
proporcoes_amostrais[i] <- sum(esta_no_intervalo)/n
#---#
i <- i + 1}

#Intervalos de confiança, e se os valores teóricos se encontram nele.
#mediana
LI_mediana <- unname(quantile(medianas_amostrais, 0.025))
LS_mediana <- unname(quantile(medianas_amostrais, 0.975))

if ((LI_mediana < mediana_teorica) && (mediana_teorica < LS_mediana)) {
	mediana_teorica_no_ic <- "Sim"
}
#propocao
LI_proporcao <- unname(quantile(proporcoes_amostrais, 0.025))
LS_proporcao <- unname(quantile(proporcoes_amostrais, 0.975))

if ((LI_proporcao < proporcao_teorica) && (proporcao_teorica < LS_proporcao)) {
	prop_teorica_no_ic <- "Sim"
}

#retornando os intervalos, e se os valores teóricos se encontram neles.
resultados <- data.frame(
	Amostras = n,
LI_mediana = round(LI_mediana,5),
LS_mediana = round(LS_mediana,5),
Acertou_med_teorica = mediana_teorica_no_ic,
LI_proporcao = round(LI_proporcao,5),
LS_proporcao = round(LS_proporcao,5),
Acertou_prop_teorica = prop_teorica_no_ic
)

resultados
}

#-----#
#simulacao
#números de amostra sendo testados
n_amostras <- c(10,50,100,500,1000,10000)

#rodando o código....
resultados_crus <- mapply(FUN = many_rep, rep = 10000, n = n_amostras, SIMPLIFY = FALSE)
resultados_cozidos <- do.call(rbind,resultados_crus)

#salvando resultados em um CSV
write.csv(resultados_cozidos, "resultados_monte_carlo.csv", row.names = FALSE)
)
}
}
}
}
}