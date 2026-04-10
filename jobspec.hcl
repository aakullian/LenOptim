variable "domain_prod" {
  type        = string
  description = "The application's production domain."
}

job "__REPO__NAME__" {
  region      = "us-west-2"
  datacenters = ["dc1"]
  type        = "service"
  namespace   = "__NAMESPACE__"

  constraint {
    attribute = attr.kernel.name
    value     = "linux"
  }

  constraint {
    attribute = node.class
    value     = "spot"
  }

  group "__REPO__NAME__" {
    count = 2
    network {
      mode = "bridge"
      port "http" { to = 3838 }
    }

    service {
      name = "__REPO__NAME__-client"
      port = "http"
      provider = "nomad"
      tags = [
        "traefik.enable=true",
        "traefik.http.routers.__REPO__NAME__.rule=Host(`${var.domain_prod}`)",
        "traefik.http.routers.__REPO__NAME__.entrypoints=https",
        "traefik.http.routers.__REPO__NAME__.tls=true",
        "traefik.http.services.__REPO__NAME__.loadbalancer.sticky=true",
        "traefik.http.services.__REPO__NAME__.loadbalancer.sticky.cookie.secure=true",
        "traefik.http.services.__REPO__NAME__.loadbalancer.sticky.cookie.httpOnly=true"
      ]
    }

    task "__REPO__NAME__-client" {
      driver = "docker"

      resources {
        cpu    = 1000
        memory = 2000
      }

      config {
        image = "bmgfsre.azurecr.io/__REPO__NAME___client:__BUILD__NUMBER__"
        ports = ["http"]
      }
    }
  }
}