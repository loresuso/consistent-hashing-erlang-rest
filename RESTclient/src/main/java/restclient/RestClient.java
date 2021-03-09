package restclient;

/*
https://eclipse-ee4j.github.io/jersey.github.io/documentation/latest/client.html
*/


import gui.GuiController;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;

import javax.ws.rs.client.*;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/*
JAX-RS Client API is designed to allow fluent programming model.
This means, a construction of a Client instance, from which a WebTarget
is created, from which a request Invocation is built and a REST HTTP request/reply
communication is built in a fully customized manner.
*/

public class RestClient extends Application {

    private static Client client;
    private static WebTarget webTarget;
    private static boolean initialized = false;
    private static String urlBase;



    public static void init(String url) {
        urlBase = url;
        initialized = true;

        /*
        To utilize the client API it is first necessary to build an instance of a
        Client using one of the static ClientBuilder factory methods.
        The ClientBuilder is a JAX-RS API used to create new instances of Client.
        In a slightly more advanced scenarios, ClientBuilder can be used to configure
        additional client instance properties, such as a SSL transport settings.
        */

        /*
        A Client instance can be configured during creation by passing a ClientConfig
        to the newClient(Configurable) ClientBuilder factory method.
        ClientConfig implements Configurable and therefore it offers methods to register
        providers (e.g. features or individual entity providers, filters or interceptors)
         */
        client = ClientBuilder.newClient( new ClientConfig().register(ClientResponseFilter.class ) );
        /*
            if the client is not able to connect to the server
            within 1000 millis its raise an exception
         */
        client.property(ClientProperties.CONNECT_TIMEOUT, 1000);
        /*
            if the client is not able to receive a response from the server
            within 1000 millis its raise an exception
         */
        client.property(ClientProperties.READ_TIMEOUT, 1000);
    }

    public static boolean isInitialized() {
        return initialized;
    }


    public static void close() {
        if(!initialized) {
            return;
        }
        client.close();
        initialized = false;
    }

    public static void setNewUrl(String newUrl) {
        close();
        init(newUrl);
    }




    public static String getByKey(String key) {
        try{
            /*
            Once you have a Client instance you can create a WebTarget from it.
            A resource target is identified by the resource URI.
            In this case the resource URI is "urlBase/kv-entries/{key}"
            */
            webTarget = client.target(urlBase).path("kv-entries").path(key);
            /*
            To start building a new HTTP request invocation, we need to create a new Invocation.Builder.
            A new invocation builder instance is created using one of the request(...) methods that
            are available on WebTarget. A couple of these methods accept parameters that let
            you define the media type of the representation requested to be returned from the
            resource. Here we are saying that we request a "application/json" type.
            This tells Jersey to add a Accept: application/json HTTP header to our request.
            */
            Invocation.Builder invocationBuilder =  webTarget.request(MediaType.APPLICATION_JSON);
            /*
            Here we are invoking the request with the GET method.
             */
            Response response = invocationBuilder.get();
            /*
            In case of successful response, with the response.readEntity(MyResponse.class) we read the json entity
            from the response and we return the specified custom MyResponse Java Object.
            */
            MyResponse myResponse = response.readEntity(MyResponse.class);
            if(myResponse.getData() == null) {
                return myResponse.getStatus();
            }
            return myResponse.getData();
        } catch (Exception e) {
            GuiController.showAlert(e.getMessage());
            return e.getMessage();
        }
    }


    public static String put(String key, String value) {
        try{
            /* In this case the resource URI is "urlBase/kv-entries" */
            webTarget = client.target(urlBase).path("kv-entries");
            MyRequest myRequest = new MyRequest(key, value);
            Invocation.Builder invocationBuilder =  webTarget.request(MediaType.APPLICATION_JSON);
            /*
            Here we are invoking the HTTP request with the PUT method.
            The first argument of the Entity.entity() function is the Java Object that will be serialized,
            the second argument tells to Entity.entity() to serialize the object into application/json.
            */
            Response response = invocationBuilder.put(Entity.entity(myRequest, MediaType.APPLICATION_JSON));
            MyResponse myResponse = response.readEntity(MyResponse.class);
            if(myResponse.getData() == null) {
                return myResponse.getStatus();
            }
            return myResponse.getData();
        } catch (Exception e) {
            GuiController.showAlert(e.getMessage());
            return e.getMessage();
        }
    }

    /* In order to understand this method, see the getByKey() comments */
    public static String deleteByKey(String key) {
        try{
            /* In this case the resource URI is "urlBase/kv-entries/{key}" */
            webTarget = client.target(urlBase).path("kv-entries").path(key);
            Invocation.Builder invocationBuilder =  webTarget.request(MediaType.APPLICATION_JSON);
            /* Here we are invoking a HTTP request with DELETE method */
            Response response = invocationBuilder.delete();
            MyResponse myResponse = response.readEntity(MyResponse.class);
            if(myResponse.getData() == null) {
                return myResponse.getStatus();
            }
            return myResponse.getData();
        } catch (Exception e) {
            GuiController.showAlert(e.getMessage());
            return e.getMessage();
        }
    }


}
