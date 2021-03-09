package it.dsmt.accessnode.restendpoints;

import it.dsmt.accessnode.kverlangconnector.KvErlangConnector;
import it.dsmt.accessnode.pojo.Request;
import it.dsmt.accessnode.pojo.Response;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Application;

// The Java class will be hosted at the URI path "/rest/kv-entries"
/**
 * This class define all the REST endpoints and the relative operations.
 * All URI path start with http://<server_ip>:<server_port>/<artifactId>-<versionId>/rest/kv-entries.
 * Sub paths are defined by the @Path annotation before function declarations.
 */
@ApplicationPath("rest")
@Path("/kv-entries")
public class KvRest extends Application {

    @GET
    @Path("/{key}")
    @Produces({MediaType.APPLICATION_JSON})
    public Response get(@PathParam("key") String key) {
        //Check if something went wrong
        if(!KvErlangConnector.getInitialized()) {
            return Response.onlyStatus("Error: cannot contact Erlang server");
        }

        //Retrieve Erlang response and return it to the client
        return KvErlangConnector.getByKey(key);
    }

    @DELETE
    @Path("/{key}")
    @Produces({MediaType.APPLICATION_JSON})
    public Response delete(@PathParam("key") String key) {
        //Check if something went wrong
        if(!KvErlangConnector.getInitialized()) {
            return Response.onlyStatus("Error: cannot contact Erlang server");
        }

        //Retrieve Erlang response and return it to the client
        return KvErlangConnector.deleteByKey(key);
    }

    @PUT
    @Produces({MediaType.APPLICATION_JSON})
    @Consumes({MediaType.APPLICATION_JSON})
    public Response put(Request request) {
        //Check if key or value are empty
        String key = request.getKey();
        String value = request.getValue();
        if(key == null || key.isEmpty() || value == null || value.isEmpty()) {
            return Response.onlyStatus("Error: cannot insert empty values or key");
        }

        //Check if something went wrong
        if(!KvErlangConnector.getInitialized()) {
            return Response.onlyStatus("Error: cannot contact Erlang server");
        }

        //Retrieve Erlang response and return it to the client
        return KvErlangConnector.insertByKey(key, value);
    }

}