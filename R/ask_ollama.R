#' Poser une Question à Ollama en Utilisant un Modèle Spécifique
#'
#' Cette fonction permet d'interroger l'API Ollama avec une question spécifique en utilisant un modèle donné.
#' Elle vérifie d'abord la disponibilité du modèle, le télécharge si nécessaire, puis envoie la requête
#' pour générer une réponse basée sur le modèle sélectionné.
#'
#' @param question \code{character} : La question ou le prompt à envoyer au modèle Ollama.
#' @param model_name \code{character} : Le nom du modèle à utiliser pour générer la réponse. Par défaut, \code{"llama2"}.
#' @param base_url \code{character} : L'URL de base de l'API Ollama. Par défaut, \code{"https://ollama-clem.lab.sspcloud.fr"}.
#'
#' @return \code{character} : La réponse générée par le modèle Ollama si la requête est réussie.
#' 
#' @details
#' La fonction effectue les opérations suivantes :
#' \itemize{
#'   \item Vérifie si le modèle spécifié est disponible en utilisant \code{\link{check_model_availability}}.
#'   \item Si le modèle n'est pas disponible, tente de le télécharger en utilisant \code{\link{pull_model}}.
#'   \item Envoie une requête POST à l'API Ollama avec le prompt fourni.
#'   \item Si la requête est réussie (code de statut HTTP 200), retourne la réponse générée par le modèle.
#'   \item Si la requête échoue, arrête l'exécution avec un message d'erreur détaillé.
#' }
#'
#' @import httr
#' @import jsonlite
#'
#' @examples
#' \dontrun{
#' # Poser une question au modèle par défaut "llama2"
#' reponse <- ask_ollama("Quelle est la capitale de la France?")
#' print(reponse)
#' 
#' # Poser une question en spécifiant un autre modèle et une URL de base personnalisée
#' reponse <- ask_ollama("Explique la théorie de la relativité.", model_name = "gpt-4", base_url = "https://custom-api.example.com")
#' print(reponse)
#' }
#'
#' @export
ask_ollama <- function(question, model_name = "llama2", base_url = "https://ollama-clem.lab.sspcloud.fr") {
    # Vérifier que le paramètre 'question' est une chaîne de caractères non vide
    if (!is.character(question) || length(question) != 1 || nchar(question) == 0) {
        stop("Le paramètre 'question' doit être une chaîne de caractères non vide.")
    }

    # Vérifier que 'model_name' est une chaîne de caractères non vide
    if (!is.character(model_name) || length(model_name) != 1 || nchar(model_name) == 0) {
        stop("Le paramètre 'model_name' doit être une chaîne de caractères non vide.")
    }

    # Vérifier que 'base_url' est une chaîne de caractères non vide
    if (!is.character(base_url) || length(base_url) != 1 || nchar(base_url) == 0) {
        stop("Le paramètre 'base_url' doit être une chaîne de caractères non vide.")
    }

    # Charger les packages nécessaires
    if (!requireNamespace("httr", quietly = TRUE)) {
        stop("Le package 'httr' est requis mais n'est pas installé.")
    }

    if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("Le package 'jsonlite' est requis mais n'est pas installé.")
    }

    # Vérifier si le modèle est disponible, sinon le télécharger
    if (!check_model_availability(model_name, base_url)) {
        message(paste("Le modèle", model_name, "n'est pas disponible. Tentative de téléchargement..."))
    if (!pull_model(model_name, base_url)) {
        stop("Impossible de télécharger le modèle. Arrêt de la fonction.")
    }
        message(paste("Le modèle", model_name, "a été téléchargé avec succès."))
    }

    # Construire l'URL de génération
    url <- paste0(base_url, "/api/generate")

    # Préparer le corps de la requête
    payload <- list(
        model = model_name,
        prompt = question,
        stream = FALSE
    )

    # Envoyer la requête POST avec le corps JSON et l'en-tête approprié
    response <- httr::POST(
        url = url, 
        body = jsonlite::toJSON(payload, auto_unbox = TRUE), 
        httr::add_headers("Content-Type" = "application/json"),
        encode = "json"
    )

    # Vérifier le code de statut de la réponse
    if (httr::status_code(response) == 200) {
        content_response <- httr::content(response, "parsed")

        # Vérifier que la réponse contient le champ attendu
        if (!is.null(content_response$response)) {
            return(content_response$response)
        } else {
            stop("La réponse de l'API ne contient pas le champ 'response'.")
        }
        } else {
            error_message <- paste("Erreur lors de la requête API:", httr::status_code(response))
            api_content <- tryCatch(
                httr::content(response, "text"),
                error = function(e) "Impossible de récupérer le contenu de la réponse."
            )
        stop(paste(error_message, "\n", api_content))
}
}
