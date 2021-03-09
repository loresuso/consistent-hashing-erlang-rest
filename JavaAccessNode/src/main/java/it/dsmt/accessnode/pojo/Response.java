package it.dsmt.accessnode.pojo;

public class Response {

    private String status;
    private String data;


    public static Response onlyData(String data) {
        Response response = new Response();
        response.setData(data);
        return response;
    }

    public static Response onlyStatus(String status) {
        Response response = new Response();
        response.setStatus(status);
        return response;
    }


    public Response() {}

    public Response(String status) {
        this.status = status;
    }

    public Response(String status, String data) {
        this.status = status;
        this.data = data;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

}
