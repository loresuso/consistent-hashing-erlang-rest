package it.dsmt.accessnode.startup;

import it.dsmt.accessnode.kverlangconnector.KvErlangConnector;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

/**
 * StartupListener implements ServletContextListener to define contextInitialized(...).
 * This method is called during the initialization phase and perform all the required operation to initialize the
 * servlet.
 */
@WebListener
public class StartupListener implements ServletContextListener {

    private final static String LOGO =
            "\n" +
            "   mmm    mm   mm   m\n" +
            "     #    ##   #\"m  #\n" +
            "     #   #  #  # #m #\n" +
            "     #   #mm#  #  # #\n" +
            " \"mmm\"  #    # #   ##";

    @Override
    public void contextInitialized(ServletContextEvent event) {
        // Perform action during application's startup
        System.out.println(LOGO);
        KvErlangConnector.init();
    }

    @Override
    public void contextDestroyed(ServletContextEvent event) {
        // Perform action during application's shutdown
        KvErlangConnector.close();
    }

}
