package com.github.prologdb.runtime.playground.jvm;

import com.github.prologdb.async.LazySequence;
import com.github.prologdb.parser.ParseException;
import com.github.prologdb.runtime.PrologRuntimeEnvironment;
import com.github.prologdb.runtime.module.*;
import com.github.prologdb.parser.Reporting;
import com.github.prologdb.parser.lexer.Lexer;
import com.github.prologdb.parser.lexer.LineEndingNormalizer;
import com.github.prologdb.parser.parser.DefaultModuleSourceFileVisitor;
import com.github.prologdb.parser.parser.ParseResult;
import com.github.prologdb.parser.parser.PrologParser;
import com.github.prologdb.parser.source.SourceUnit;
import com.github.prologdb.runtime.DefaultPrologRuntimeEnvironment;
import com.github.prologdb.runtime.PrologException;
import com.github.prologdb.runtime.module.Module;
import com.github.prologdb.runtime.playground.jvm.editor.PrologEditorPanel;
import com.github.prologdb.runtime.playground.jvm.persistence.PlaygroundState;
import com.github.prologdb.runtime.proofsearch.ProofSearchContext;
import com.github.prologdb.runtime.proofsearch.ReadWriteAuthorization;
import com.github.prologdb.runtime.query.Query;
import com.github.prologdb.runtime.stdlib.loader.StandardLibraryModuleLoader;
import com.github.prologdb.runtime.unification.Unification;
import com.github.prologdb.runtime.unification.VariableBucket;
import com.github.prologdb.runtime.util.EmptyOperatorRegistry;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.time.Duration;
import java.util.Collections;

import static java.util.stream.Collectors.joining;

public class PlaygroundPanel {

    private static final ModuleReference USER_MODULE_REFERENCE = new ModuleReference("playground", "user");

    private JPanel panel = new JPanel(new BorderLayout());
    private PrologEditorPanel knowledgeBaseEditorPanel;
    private QueryPanel queryPanel;
    private SolutionExplorerPanel solutionExplorerPanel;

    private PrologParser parser = new PrologParser();

    /**
     * Is set back to false in {@link #assureKnowledgeBaseIsUpToDate()}.
     */
    private boolean knowledgeBaseChangeIndicator = false;

    private final ModuleLoader moduleLoader = new CascadingModuleLoader(new PlaygroundModuleLoader(), StandardLibraryModuleLoader.INSTANCE);

    private DefaultPrologRuntimeEnvironment runtimeEnvironment = null;

    public PlaygroundPanel() {
        initComponents();
    }

    public PrologParser getParser() {
        return parser;
    }

    public PlaygroundPanel setParser(PrologParser parser) {
        this.parser = parser;
        this.queryPanel.setParser(parser);
        return this;
    }

    public JPanel asJPanel() {
        return panel;
    }

    /** @return the current state of the playground */
    public PlaygroundState getCurrentState() {
        PlaygroundState state = new PlaygroundState();
        state.setKnowledgeBaseText(knowledgeBaseEditorPanel.getCodeAsString());
        state.setQuery(queryPanel.getCodeAsString());

        return state;
    }

    public void setCurrentState(PlaygroundState state) {
        knowledgeBaseEditorPanel.setCodeAsString(state.getKnowledgeBaseText());
        queryPanel.setCodeAsString(state.getQuery());
    }

    private void initComponents() {
        knowledgeBaseEditorPanel = new PrologEditorPanel();
        queryPanel = new QueryPanel();
        solutionExplorerPanel = new SolutionExplorerPanel();

        JSplitPane queryAndResultsSplitter = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        queryAndResultsSplitter.add(queryPanel.asJPanel());
        queryAndResultsSplitter.add(solutionExplorerPanel.asJPanel());
        queryAndResultsSplitter.setBorder(null);

        JSplitPane baseAndQuerySplitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        baseAndQuerySplitter.add(knowledgeBaseEditorPanel.asJPanel());
        baseAndQuerySplitter.add(queryAndResultsSplitter);
        baseAndQuerySplitter.setBorder(null);

        panel.add(baseAndQuerySplitter, BorderLayout.CENTER);

        // -- listeners
        knowledgeBaseEditorPanel.addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(KeyEvent e) {}

            @Override
            public void keyPressed(KeyEvent e) { }

            @Override
            public void keyReleased(KeyEvent e) {
                knowledgeBaseChangeIndicator = true;
            }
        });

        queryPanel.addQueryFiredListener((panel, query) -> onQueryFired(query));
    }

    private void assureKnowledgeBaseIsUpToDate() throws ParseException, PrologException
    {
        if (runtimeEnvironment != null && !knowledgeBaseChangeIndicator) {
            return;
        }

        long loadStart = System.nanoTime();
        DefaultPrologRuntimeEnvironment newRuntime = new DefaultPrologRuntimeEnvironment(moduleLoader);
        newRuntime.assureModulePrimed(USER_MODULE_REFERENCE);
        newRuntime.getFullyLoadedModule(USER_MODULE_REFERENCE.getModuleName());
        solutionExplorerPanel.setParseTime(Duration.ofNanos(System.nanoTime() - loadStart));
        runtimeEnvironment = newRuntime;
        knowledgeBaseChangeIndicator = false;
    }

    private void onQueryFired(String queryCode) {
        try {
            assureKnowledgeBaseIsUpToDate();
        }
        catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, ex.getMessage(), "Error in knowledge base", JOptionPane.ERROR_MESSAGE);
            ex.printStackTrace();
            return;
        } catch (PrologException ex) {
            JOptionPane.showMessageDialog(null, ex.getMessage(), "Failed to load user knowledge base", JOptionPane.ERROR_MESSAGE);
            ex.printStackTrace();
            return;
        }

        Module userModule = runtimeEnvironment.getFullyLoadedModule(USER_MODULE_REFERENCE.getModuleName());
        Lexer queryLexer = new Lexer(
            new SourceUnit("query"),
            new LineEndingNormalizer(
                new CharacterIterable(queryCode).iterator()
            )
        );
        ParseResult<Query> queryParseResult = parser.parseQuery(queryLexer, userModule.getLocalOperators());
        if (!queryParseResult.getReportings().isEmpty() || queryParseResult.getItem() == null) {
            JOptionPane.showMessageDialog(
                null,
                "Errors in query:\n" + queryParseResult.getReportings().stream().map(Reporting::toString).collect(joining(" \n")),
                "Error in query",
                JOptionPane.ERROR_MESSAGE
            );
            return;
        }

        ProofSearchContext psc = runtimeEnvironment.newProofSearchContext(userModule.getDeclaration().getModuleName(), ReadWriteAuthorization.INSTANCE);
        LazySequence<Unification> solutions = ProofSearchContext.fulfill(psc, queryParseResult.getItem(), new VariableBucket());

        solutionExplorerPanel.setSolutions(solutions, userModule.getLocalOperators());
        solutionExplorerPanel.showNextSolution();
        this.panel.revalidate();
        this.panel.repaint();
    }

    private class PlaygroundModuleLoader implements ModuleLoader {
        @NotNull
        @Override
        public PrimedStage initiateLoading(@NotNull ModuleReference reference, @NotNull PrologRuntimeEnvironment runtime) {
            if (!USER_MODULE_REFERENCE.equals(reference)) {
                throw new ModuleNotFoundException(reference, null);
            }

            Lexer lexer = new Lexer(
                new SourceUnit("knowledge_base"),
                new LineEndingNormalizer(
                    new CharacterIterable(
                        knowledgeBaseEditorPanel.getCodeAsString()
                    ).iterator()
                )
            );

            return parser.parseSourceFile(
                lexer,
                new DefaultModuleSourceFileVisitor(runtime),
                new ModuleDeclaration("user", Collections.emptySet(), EmptyOperatorRegistry.INSTANCE)
            );
        }
    }
}
