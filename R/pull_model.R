#' Télécharger un Modèle via une API
#'
#' Cette fonction permet de télécharger un modèle spécifique en envoyant une requête POST à une API donnée. Elle envoie le nom du modèle dans le corps de la requête au format JSON et gère la réponse pour déterminer si le téléchargement a réussi.
#'
#' @param model_name \code{character} : Le nom du modèle à télécharger.
#' @param base_url \code{character} : L'URL de base de l'API à interroger. Par défaut, \code{"https://ollama-clem.lab.sspcloud.fr"}.
#'
#' @return \code{logical} : \code{TRUE} si le modèle a été téléchargé avec succès (code de statut HTTP 200), \code{FALSE} sinon.
#'
#' @details
#' La fonction effectue les opérations suivantes :
#' \itemize{
#'   \item Construit l'URL de téléchargement en ajoutant \code{/api/pull} à \code{base_url}.
#'   \item Prépare le corps de la requête avec le nom du modèle au format JSON.
#'   \item Envoie une requête POST à l'API avec le corps JSON et l'en-tête \code{Content-Type: application/json}.
#'   \item Vérifie le code de statut de la réponse :
#'     \itemize{
#'       \item \code{200} : Le téléchargement a réussi. Affiche un message de succès et retourne \code{TRUE}.
#'       \item Autre : Affiche un message d'erreur avec le code de statut et le contenu de la réponse, puis retourne \code{FALSE}.
#'     }
#' }
#'
#' @import httr
#' @import jsonlite
#'
#' @examples
#' \dontrun{
#' # Télécharger un modèle nommé "gpt-4"
#' succes <- pull_model("gpt-4")
#' if (succes) {
#'   message("Le modèle 'gpt-4' a été téléchargé avec succès.")
#' } else {
#'   message("Le téléchargement du modèle 'gpt-4' a échoué.")
#' }
#'
#' # Télécharger un modèle avec une URL de base personnalisée
#' succes <- pull_model("gpt-4", base_url = "https://custom-api.example.com")
#' }
#'
#' @export
pull_model <- function(model_name, base_url = "https://ollama-clem.lab.sspcloud.fr") {

    if (!is.character(model_name) || length(model_name) != 1) {
        stop("Le paramètre 'model_name' doit être une chaîne de caractères unique.")
    }

    if (!is.character(base_url) || length(base_url) != 1) {
        stop("Le paramètre 'base_url' doit être une chaîne de caractères unique.")
    }

    # Construire l'URL de téléchargement
    pull_url <- paste0(base_url, "/api/pull")

    # Préparer le corps de la requête
    payload <- list(name = model_name)

    # Envoyer la requête POST avec le corps JSON et l'en-tête approprié
    response <- httr::POST(
        url = pull_url, 
        body = jsonlite::toJSON(payload, auto_unbox = TRUE),
        httr::add_headers("Content-Type" = "application/json"),
        encode = "json"
    )

    # Vérifier le code de statut de la réponse
    if (httr::status_code(response) == 200) {
        message(paste("Modèle", model_name, "téléchargé avec succès."))
        return(TRUE)
    } else {
        message(paste("Erreur lors du téléchargement du modèle :", httr::status_code(response)))
        message(httr::content(response, "text"))
    return(FALSE)
    }
}
