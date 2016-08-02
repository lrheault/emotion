package edu.stanford.nlp.pipeline;

import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.IndexedWord;
import edu.stanford.nlp.semgraph.SemanticGraph;
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations;
import edu.stanford.nlp.semgraph.SemanticGraphEdge;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.StringUtils;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * <p>Write a subset of our CoreNLP output in CoNLL format.</p>
 *
 * <p>The fields currently output are:</p>
 *
 * <table>
 *   <tr>
 *     <td>Field Number</td>
 *     <td>Field Name</td>
 *     <td>Description</td>
 *   </tr>
 *   <tr>
 *     <td>1</td>
 *     <td>ID</td>
 *     <td>Token Counter, starting at 1 for each new sentence.</td>
 *   </tr>
 *   <tr>
 *     <td>2</td>
 *     <td>FORM</td>
 *     <td>Word form or punctuation symbol.</td>
 *   </tr>
 *   <tr>
 *     <td>3</td>
 *     <td>LEMMA</td>
 *     <td>Lemma of word form, or an underscore if not available.</td>
 *   </tr>
 *   <tr>
 *     <td>4</td>
 *     <td>POSTAG</td>
 *     <td>Fine-grained part-of-speech tag, or underscore if not available.</td>
 *   </tr>
 *   <tr>
 *     <td>5</td>
 *     <td>NER</td>
 *     <td>Named Entity tag, or underscore if not available.</td>
 *   </tr>
 *   <tr>
 *     <td>6</td>
 *     <td>HEAD</td>
 *     <td>Head of the current token, which is either a value of ID or zero ('0').
 *         This is underscore if not available.</td>
 *   </tr>
 *   <tr>
 *     <td>7</td>
 *     <td>DEPREL</td>
 *     <td>Dependency relation to the HEAD, or underscore if not available.</td>
 *   </tr>
 * </table>
 *
 * @author Gabor Angeli
 */
public class CoNLLOutputter extends AnnotationOutputter {

  private static final String NULL_PLACEHOLDER = "_";

  public CoNLLOutputter() { }

  private static String orNull(String in) {
    if (in == null) {
      return NULL_PLACEHOLDER;
    } else {
      return in;
    }
  }

  /**
   * Produce a line of the CoNLL output.
   */
  private static String line(int linenumber, int sentCount, int index, 
                      CoreLabel token,
                      int head, String deprel) {
    ArrayList<String> fields = new ArrayList<>(16);

	fields.add(Integer.toString(linenumber)); // 0
    fields.add(Integer.toString(sentCount)); // 1
    fields.add(Integer.toString(index)); // 2
    fields.add(orNull(token.word()));    // 3
    fields.add(orNull(token.lemma()));   // 4
    fields.add(orNull(token.tag()));     // 5
    // fields.add(orNull(token.ner()));     // 6
    // if (head >= 0) {
    //  fields.add(Integer.toString(head));  // 7
    //  fields.add(deprel);                  // 8
    // } else {
    //  fields.add(NULL_PLACEHOLDER);
    //  fields.add(NULL_PLACEHOLDER);
    // }

    return StringUtils.join(fields, "\t");
  }

  @Override
  public void print(Annotation doc, OutputStream target, Options options) throws IOException {
    PrintWriter writer = new PrintWriter(target);

    // vv A bunch of nonsense to get tokens vv
    if (doc.get(CoreAnnotations.SentencesAnnotation.class) != null) {
	  int sentCount = 1;
      for (CoreMap sentence : doc.get(CoreAnnotations.SentencesAnnotation.class)) {
        
        if (sentence.get(CoreAnnotations.TokensAnnotation.class) != null) {
           
          List<CoreLabel> tokens = sentence.get(CoreAnnotations.TokensAnnotation.class);
		  Integer linenumber = sentence.get(CoreAnnotations.LineNumberAnnotation.class);
          // Integer sentnumber = sentence.get(CoreAnnotations.TokensAnnotation.class);
          SemanticGraph depTree = sentence.get(SemanticGraphCoreAnnotations.BasicDependenciesAnnotation.class);
          for (int i = 0; i < tokens.size(); ++i) {
            // ^^ end nonsense to get tokens ^^

            // Newline if applicable
            if (i > 0) {
              writer.println();
            }

            // Try to get the incoming dependency edge
            int head = -1;
            String deprel = null;
            if (depTree != null) {
              Set<Integer> rootSet = depTree.getRoots().stream().map(IndexedWord::index).collect(Collectors.toSet());
              IndexedWord node = depTree.getNodeByIndexSafe(i + 1);
              if (node != null) {
                List<SemanticGraphEdge> edgeList = depTree.getIncomingEdgesSorted(node);
                if (!edgeList.isEmpty()) {
                  assert edgeList.size() == 1;
                  head = edgeList.get(0).getGovernor().index();
                  deprel = edgeList.get(0).getRelation().toString();
                } else if (rootSet.contains(i + 1)) {
                  head = 0;
                  deprel = "ROOT";
                }
              }
            }

            // Write the token
            writer.print(line(linenumber, sentCount, i + 1, tokens.get(i), head, deprel));
			
          }
        sentCount++;  
        }
        
        writer.println();
        // I believe this is the redundant extra line.
        // writer.println();
      }
    
    }
    writer.flush();
  }

  public static void conllPrint(Annotation annotation, OutputStream os) throws IOException {
    new CoNLLOutputter().print(annotation, os);
  }

  public static void conllPrint(Annotation annotation, OutputStream os, StanfordCoreNLP pipeline) throws IOException {
    new CoNLLOutputter().print(annotation, os, pipeline);
  }

  public static void conllPrint(Annotation annotation, OutputStream os, Options options) throws IOException {
    new CoNLLOutputter().print(annotation, os, options);
  }

}
